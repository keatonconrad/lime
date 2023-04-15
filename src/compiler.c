#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "list.h"
#include "ast.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
    List* tokens;
    int tokenIndex;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY
} Precedence;

typedef ASTNode* (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix; // Fn to compile prefix expression starting with token of that type
    ParseFn infix; // Fn to compile infix expression whose left operand is followed by token of that type
    Precedence precedence; // Precedence of infix expression that uses that token as an operator
} ParseRule;

// Stores local variable
typedef struct {
    Token name; // The name of the variable
    int depth; // The scope depth of the block where the local variable was declared
    bool isCaptured; // Whether the local variable is captured by a closure
} Local;

typedef struct {
    uint8_t index; // Stores which local slot the upvalue is capturing
    bool isLocal; // Whether the closure captures a local variable or an upvalue from the surrounding function
} Upvalue;

// Stores local variable state
typedef struct _Compiler {
    struct _Compiler* enclosing;
    ObjFunction* function; // A reference to the function object being built
    FunctionType type; // Signals function body vs top-level code

    Local locals[UINT8_COUNT];
    int localCount; // How many local variables are in scope
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth; // Number of blocks surrounding the current bit of code being compiled
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool hasSuperclass;
} ClassCompiler;

typedef struct {
	bool isInLoop;
	int loopStart; // Stores current loop starting position to use with continue.
	int jumpToExit; // Stores break statement JUMP to patch for exit. This allows only one break per loop
} LoopMetadata;

Parser parser;
Compiler* current = NULL;
ClassCompiler* currentClass = NULL;
LoopMetadata loopMetadata;

static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        if (parser.tokenIndex >= parser.tokens->count) {
            parser.current.type = TOKEN_EOF;
            break;
        }

        Token* token = (Token*)listGet(parser.tokens, parser.tokenIndex);
        parser.current = *token;
        parser.tokenIndex++;

        // Debug prints
        printf("Previous token: %s\n", tokenTypeToString(parser.previous.type));
        printf("Current token: %s\n", tokenTypeToString(parser.current.type));

        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}
 

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

// Checks that the current token's TokenType == type
static bool check(TokenType type) {
    return parser.current.type == type;
}

// If the current token has the given type, consume the token and return true
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

// Sends in previous token's line information so that runtime
// errors are associated with that line
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    // The + 2 is to take into account the size of the OP_LOOP instruction's
    // own operands which we also need to jump over
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // Two bytes (16-bit offset) lets us jump over up to 65,535 bytes of code
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void initLoopMetadata() {
	loopMetadata.isInLoop = false;
	loopMetadata.loopStart = -1;
	loopMetadata.jumpToExit = -1;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start,
                                             parser.previous.length);
    }

    // The compiler claims stack slot zero for the VM's own internal use
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
    return function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL
            ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    uint8_t popCounter = 0;
    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
    if (popCounter == 1) emitByte(OP_POP);
    else if (popCounter > 0) emitBytes(OP_POPN, popCounter);
}

static ASTNode* expression();
static ASTNode* statement();
static ASTNode* declaration();
static ASTNode* and_(bool canAssign);
static int resolveLocal(Compiler* compiler, Token* name);
static int resolveUpvalue(Compiler* compiler, Token* name);
static ParseRule* getRule(TokenType type);
static ASTNode* parsePrecedence(Precedence Precedence);

static ASTNode* binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    ASTNode* left = expression();
    ASTNode* right = expression();
    
    return new_binary_node(operatorType, left, right);
}


static uint8_t parseArgumentList(ASTNode* callNode, List arguments) {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
                break;
            }
            ASTNode* argumentNode = expression();
            writeList(callNode->as.call.arguments, argumentNode);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static ASTNode* dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    Token name = parser.previous;
    ASTNode* object;
    ASTNode* value;
    NodeType nodeType;
    List arguments;
    uint8_t argCount = 0;

    if (canAssign && match(TOKEN_EQUAL)) {
        value = expression();
        nodeType = NODE_SET_PROPERTY;
    } else if (match(TOKEN_LEFT_PAREN)) {
        nodeType = NODE_INVOKE;
    } else {
        nodeType = NODE_GET_PROPERTY;
    }

    ASTNode* node;
    node->type = nodeType;
    switch (nodeType) {
        case NODE_SET_PROPERTY: {
            node = new_set_property_node(object, name, value);
            break;
        }
        case NODE_INVOKE: {
            node = new_invoke_node(object, name);
            break;
        }
        case NODE_GET_PROPERTY: {
            node = new_get_property_node(object, name);
            break;
        }
        default: break; // Unreachable
    }

    if (nodeType == NODE_INVOKE) {
        argCount = parseArgumentList(node, arguments);
    }

    return node;
}


static ASTNode* literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: return new_literal_node(TOKEN_FALSE);
        case TOKEN_NIL: return new_literal_node(TOKEN_NIL);
        case TOKEN_TRUE: return new_literal_node(TOKEN_TRUE);
        default: return NULL; // Unreachable
    }
}

static ASTNode* grouping(bool canAssign) {
    printf("grouping\n");
    ASTNode* node = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    return node;
}

static ASTNode* number(bool canAssign) {
    printf("it's a number!\n");
    double value = strtod(parser.previous.start, NULL);
    return new_number_node(value);
}

static ASTNode* or_(bool canAssign) {
    ASTNode* left = expression();
    ASTNode* right = expression();
    return new_logical_node(TOKEN_OR, left, right);
}

static ASTNode* string(bool canAssign) {
    // Start at index 1 to skip the opening quotation mark.
    int inputIndex = 1;
    int outputIndex = 0;

    // Allocate space for the processed string.
    // The length is equal to the lexeme length minus the quotation marks.
    int outputLength = parser.previous.length - 2;
    char* processedString = (char*)malloc(outputLength + 1);  // +1 for null terminator

    // Process the string, handling escape sequences.
    while (inputIndex < parser.previous.length - 1) {  // Stop before the closing quotation mark
        char current = parser.previous.start[inputIndex];
        if (current == '\\') {  // Escape sequence
            inputIndex++;  // Move to the next character after the backslash
            char escaped = parser.previous.start[inputIndex];

            switch (escaped) {
                case 'n':
                    processedString[outputIndex++] = '\n';
                    break;
                case 'r':
                    processedString[outputIndex++] = '\r';
                    break;
                case 't':
                    processedString[outputIndex++] = '\t';
                    break;
                case 'b':
                    processedString[outputIndex++] = '\b';
                    break;
                case 'f':
                    processedString[outputIndex++] = '\f';
                    break;
                default:
                    // Copy the character after the backslash without processing
                    processedString[outputIndex++] = '\\';
                    processedString[outputIndex++] = escaped;
            }
        } else {
            processedString[outputIndex++] = current;
        }
        inputIndex++;
    }

    // Add null terminator to the processed string
    processedString[outputIndex] = '\0';

    // Emit the constant with the processed string
    return new_string_node(processedString);
}


static ASTNode* namedVariable(Token name, bool canAssign) {
    VariableAccessType accessType;
    int arg = resolveLocal(current, &name);
    printf("Variable name: ");
    printToken(&name);
    printf("\n");

    if (arg != -1) {
        accessType = ACCESS_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        accessType = ACCESS_UPVALUE;
    } else {
        accessType = ACCESS_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        ASTNode* value = expression();
        return new_variable_assignment_node(name, accessType, arg, value);
    } else {
        return new_variable_access_node(name, accessType, arg);
    }
}


static ASTNode* variable(bool canAssign) {
    return namedVariable(parser.previous, canAssign);
}

static ASTNode* call(bool canAssign) {
    printf("call called\n");
    Token calleeToken = *(Token*)listGet(parser.tokens, parser.tokenIndex - 3);
    ASTNode* callee = new_variable_access_node(calleeToken, ACCESS_GLOBAL, -1);
    printf("printing callee \n");
    print_ast(callee);
    ASTNode* callNode = new_call_node(callee);
    List arguments;
    initList(&arguments);
    uint8_t argCount = parseArgumentList(callNode, arguments);

    return callNode;
}

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}


static ASTNode* super_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'super' outside of a class.");
    } else if (!currentClass->hasSuperclass) {
        error("Can't use 'super' in a class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
    Token name = parser.previous;
    // Generates code to look up the current receiver stored in "this" and push it onto the stack
    ASTNode* thisNamedVariable = namedVariable(syntheticToken("this"), false);

    List arguments;

    if (match(TOKEN_LEFT_PAREN)) {
        ASTNode* superCallNode = new_super_call_node(name);
        initList(&arguments);
        uint8_t argCount = parseArgumentList(superCallNode, arguments);

        ASTNode* superNamedVariable = namedVariable(syntheticToken("super"), false);
        return superCallNode;
    } else {
        // Generates code to look up the superclass from "super" and push it onto the stack
        ASTNode* superNamedVariable = namedVariable(syntheticToken("super"), false);
        return new_super_property_access_node(name);
    }
}

static ASTNode* this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
    }
    
    return variable(false);
}

static ASTNode* unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand
    ASTNode* right = parsePrecedence(PREC_UNARY);

    return new_unary_node(operatorType, right);
}

// Maps a ParseRule to the index of the token type in TokenType
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
    [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_BREAK]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_CONTINUE]      = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static ASTNode* parsePrecedence(Precedence precedence) {
    // Starts at the current token and parses any expression at
    // the given precedence level or higher
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return NULL;
    }

    // variable() should look for and consume the = only if it's in
    // the context of a low-precedence expression
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    ASTNode* leftNode = prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        leftNode = infixRule(canAssign);
        printf("leftnode\n");
        print_ast(leftNode);

        if (canAssign && match(TOKEN_EQUAL)) {
            error("Invalid assignment target.");
        }
    }
    return leftNode;
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

// Walks the list of locals that are currently in scope. If one has
// the same name as the identifier token, return its index
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                // Catches examples like    var a = a
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    // If the local isn't found, signal it must be a global by returning -1
    return -1;
}

static ASTNode* addLocal(Token nameToken) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return NULL;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = nameToken;
    local->depth = -1; // Marks it as uninitialized
    local->isCaptured = false;



    return (ASTNode*)local;
}


static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    // If we find an upvalue in the array whose slot index matches the one
    // we're adding, return that *upvalue* index and reuse it
    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    // The index field tracks the closed-over local variable's slot index
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

// Looks for a local variable declared in any of the surrounding functions.
// If it finds one, it returns an "upvalue index" for that variable, or -1
static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;
    
    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) return addUpvalue(compiler, (uint8_t)upvalue, false);

    return -1;
}

// Adds a local variable to the scope
static ASTNode* declareVariable() {
    printf("declareVariable1\n");
    if (current->scopeDepth == 0) return NULL;

    Token* name = &parser.previous;
    printf("Variable name: ");
    printToken(name);
    printf("\n");
    for (int i = current->localCount - 1; i >= 0; i--) {
        // When we declare a new variable, we start at the end and
        // work backward, looking for an existing variable with the
        // same name
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }
    printf("declareVariable\n");
    return addLocal(*name);
}

// Consumes the identifier token for the variable name, adds its lexeme
// to the chunk's constant table as a string, and then returns the constant
// table index where it was added
static Token* parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);
    printf("parseVariable\n");

    ASTNode* variable = declareVariable();
    // If we're in a local scope, exit the function by returning a
    // dummy index. Locals aren't looked up by name at runtime
    // if (current->scopeDepth > 0) return 0;

    return &parser.previous;
}

static ASTNode* and_(bool canAssign) {
    ASTNode* left = expression();
    ASTNode* right = expression();
    return new_logical_node(TOKEN_AND, left, right);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static ASTNode* expression() {
    // Parse the lowest precedence level
    return parsePrecedence(PREC_ASSIGNMENT);
}

static ASTNode* block() {
    ASTNode* node = new_block_node();
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        node->as.block.statements[node->as.block.statement_count++] = declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    return node;
}

static ASTNode* function(FunctionType type) {
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            Token* constant = parseVariable("Expect parameter name.");
            // defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");

    ASTNode* body = block();
    ASTNode* node = new_function_node(parser.previous, current->function->arity, type, body);

    endScope();

    return node;
}


static ASTNode* method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    ASTNode* body = function(type);
    return new_function_node(parser.previous, current->function->arity, type, body);
}

static ASTNode* classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    printf("className: ");
    printToken(&className);
    printf("\n");
    declareVariable();

    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    // If we see a "<" token, we're inheriting from another class
    Token superclass = syntheticToken(""); // Empty superclass token
    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        superclass = parser.previous;
        variable(false); // Looks up the superclass by name and pushes it onto the stack
        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself.");
        }

        beginScope();
        ASTNode* localSuper = addLocal(syntheticToken("super"));
        // defineVariable(0);

        ASTNode* classNamedVariable = namedVariable(className, false); // Loads the subclass onto the stack
        classCompiler.hasSuperclass = true;
    }

    ASTNode* classNamedVariable2 = namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");

    List methods;
    initList(&methods);
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        ASTNode* methodNode = method();
        writeList(&methods, methodNode);
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");

    if (classCompiler.hasSuperclass) {
        endScope();
    }
    currentClass = currentClass->enclosing;

    return new_class_declaration_node(className, superclass, classCompiler.hasSuperclass, methods);
}

static ASTNode* funDeclaration() {
    Token* global = parseVariable("Expect function name.");
    ASTNode* node = function(TYPE_FUNCTION);
    // defineVariable(global);
    return node;
}

static ASTNode* varDeclaration() {
    Token variableName = *parseVariable("Expect variable name.");
    printf("variableName: ");
    printToken(&variableName);
    printf("\n");
    
    ASTNode* value = malloc(sizeof(ASTNode));
    if (match(TOKEN_EQUAL)) {
        value = expression();
    } else {
        value = new_literal_node(TOKEN_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
    ASTNode* result = new_variable_assignment_node(variableName, ACCESS_GLOBAL, 0, value);
    // defineVariable(global);
    return result;
}

static ASTNode* expressionStatement() {
    ASTNode* node = expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    return node;
}

static ASTNode* ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    ASTNode* condition = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    ASTNode* thenBranch = block();

    ASTNode* elseBranch = NULL;
    if (match(TOKEN_ELSE)) {
        elseBranch = block();
    }

    return new_if_statement_node(condition, thenBranch, elseBranch);
}

static ASTNode* returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    ASTNode* node = NULL;
    if (match(TOKEN_SEMICOLON)) {
        node = new_return_statement_node(NULL);
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }
        ASTNode* expr = expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        node = new_return_statement_node(expr);
    }
    return node;
}

static ASTNode* continueStatement() {
    consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'.");
    if (!loopMetadata.isInLoop) {
        error("Invalid continue statement outside loop.");
    }
    if (loopMetadata.loopStart != -1) {
        error("Only one continue statement per loop is allowed.");
    }
    int offset = currentChunk()->count - loopMetadata.loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");
    ASTNode* node = new_continue_statement_node(offset);
    return node;
}

static ASTNode* breakStatement() {
    consume(TOKEN_SEMICOLON, "Expect ';' after 'break'.");
    if (!loopMetadata.isInLoop) {
        error("Invalid break statement outside loop.");
    }
    if (loopMetadata.jumpToExit != -1) {
        error("Only one break statement per loop is allowed.");
    }
    ASTNode* node = new_break_statement_node();
    return node;
}


static ASTNode* forStatement() {
    ASTNode* forNode = malloc(sizeof(ASTNode));
    forNode->type = NODE_FOR_STATEMENT;
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    // Handles the initializer clause
    if (match(TOKEN_SEMICOLON)) {
        forNode->as.for_statement.initializer = NULL;
    } else if (match(TOKEN_VAR)) {
        forNode->as.for_statement.initializer = varDeclaration();
    } else {
        forNode->as.for_statement.initializer = expressionStatement();
    }

    // Handles the condition clause
    if (match(TOKEN_SEMICOLON)) {
        forNode->as.for_statement.condition = NULL;
    } else {
        forNode->as.for_statement.condition = expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");
    }

    // Handles the increment clause
    if (match(TOKEN_RIGHT_PAREN)) {
        forNode->as.for_statement.increment = NULL;
    } else {
        forNode->as.for_statement.increment = expression();
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
    }

    forNode->as.for_statement.body = statement();

    endScope();
    return forNode;
}

static ASTNode* whileStatement() {
    ASTNode* whileNode = malloc(sizeof(ASTNode));
    whileNode->type = NODE_WHILE_STATEMENT;

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    whileNode->as.while_statement.condition = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    whileNode->as.while_statement.body = statement();

    return whileNode;
}


// Skips tokens indiscriminately until it reaches something that looks
// like a statement boundary.
static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
                return;

            default:
                ; // Do nothing
        }

        advance();
    }
}

static ASTNode* declaration() {
    if (match(TOKEN_CLASS)) {
        return classDeclaration();
    } else if (match(TOKEN_FUN)) {
        return funDeclaration();
    } else if (match(TOKEN_VAR)) {
        printf("should be var declaration\n");
        return varDeclaration();
    } else {
        return statement();
    }

    if (parser.panicMode) synchronize();
}

static ASTNode* statement() {
    if (match(TOKEN_IF)) {
        return ifStatement();
    } else if (match(TOKEN_BREAK)) {
        return breakStatement();
    } else if (match(TOKEN_CONTINUE)) {
        return continueStatement();
    } else if (match(TOKEN_WHILE)) {
        return whileStatement();
    } else if (match(TOKEN_FOR)) {
        return forStatement();
    } else if (match(TOKEN_RETURN)) {
        return returnStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        ASTNode* result = block();
        endScope();
        return result;
    } else {
        return expressionStatement();
    }
}

void initParser(List* tokens) {
    parser.tokens = tokens;
    parser.tokenIndex = 0;
    parser.hadError = false;
    parser.panicMode = false;

    parser.previous.type = TOKEN_ERROR;
    parser.previous.start = "";
    parser.previous.length = 0;
    parser.previous.line = 0;

    parser.current.type = TOKEN_ERROR;
    parser.current.start = "";
    parser.current.length = 0;
    parser.current.line = 0;
}

ASTNode* compile(const char* source) {
    List tokens;
    initList(&tokens);
    scanTokens(source, &tokens);
    printf("Tokens:\n");
    printTokens(&tokens);

    initParser(&tokens);

    initLoopMetadata();
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    printf("\nfirst token: %s\n", tokenTypeToString((*(Token*)listGet(&tokens, 0)).type));
    printf("first token2: %s\n", tokenTypeToString(parser.previous.type));
    
    advance();
    
    ASTNode* node;
    while (!match(TOKEN_EOF)) {
        node = declaration();
    }
    print_ast(node);

    // ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : node;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}