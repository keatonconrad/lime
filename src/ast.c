#include <stdbool.h>
#include <stdio.h>

#include "list.h"
#include "ast.h"
#include "compiler.h"

ASTNode* new_binary_node(TokenType operator, ASTNode* left, ASTNode* right) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BINARY;
    node->as.binary.operator = operator;
    node->as.binary.left = left;
    node->as.binary.right = right;
    return node;
}

ASTNode* new_call_node(ASTNode* callee, ASTNode** arguments, int arg_count) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_CALL;
    node->as.call.callee = callee;
    node->as.call.arguments = arguments;
    node->as.call.arg_count = arg_count;
    return node;
}

ASTNode* new_get_property_node(ASTNode* object, Token name) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_GET_PROPERTY;
    node->as.get_property.object = object;
    node->as.get_property.name = name;
    return node;
}

ASTNode* new_set_property_node(ASTNode* object, Token name, ASTNode* value) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_SET_PROPERTY;
    node->as.set_property.object = object;
    node->as.set_property.name = name;
    node->as.set_property.value = value;
    return node;
}

ASTNode* new_invoke_node(ASTNode* object, Token name, ASTNode** arguments, int arg_count) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_INVOKE;
    node->as.invoke.object = object;
    node->as.invoke.name = name;
    node->as.invoke.arguments = arguments;
    node->as.invoke.arg_count = arg_count;
    return node;
}

ASTNode* new_literal_node(TokenType token_type) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_LITERAL;
    node->as.literal.token_type = token_type;
    return node;
}

ASTNode* new_number_node(double value) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_NUMBER;
    node->as.number.value = value;
    return node;
}

ASTNode* new_unary_node(TokenType operator, ASTNode* operand) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_UNARY;
    node->as.unary.operator = operator;
    node->as.unary.operand = operand;
    return node;
}

ASTNode* new_logical_node(TokenType operator, ASTNode* left, ASTNode* right) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_LOGICAL;
    node->as.logical.operator = operator;
    node->as.logical.left = left;
    node->as.logical.right = right;
    return node;
}

ASTNode* new_grouping_node(ASTNode* expression) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_GROUPING;
    node->as.grouping.expression = expression;
    return node;
}


ASTNode* new_expression_statement_node(ASTNode* expression) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_EXPRESSION_STATEMENT;
    node->as.expression_statement.expression = expression;
    return node;
}

ASTNode* new_block_node() {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BLOCK;
    node->as.block.statements = NULL;
    node->as.block.statement_count = 0;
    return node;
}


ASTNode* new_return_statement_node(ASTNode* value) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_RETURN_STATEMENT;
    node->as.return_statement.value = value;
    return node;
}

ASTNode* new_class_declaration_node(Token className, Token superclass, bool hasSuperclass, List methods) {
    ASTNode* node = (ASTNode*) malloc(sizeof(ASTNode));
    node->type = NODE_CLASS_DECLARATION;
    node->as.classDeclaration.className = className;
    node->as.classDeclaration.superclass = superclass;
    node->as.classDeclaration.hasSuperclass = hasSuperclass;
    node->as.classDeclaration.methods = methods;
    return node;
}


ASTNode* new_if_statement_node(ASTNode* condition, ASTNode* then_branch, ASTNode* else_branch) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_IF_STATEMENT;
    node->as.if_statement.condition = condition;
    node->as.if_statement.then_branch = then_branch;
    node->as.if_statement.else_branch = else_branch;
    return node;
}

ASTNode* new_while_statement_node(ASTNode* condition, ASTNode* body) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_WHILE_STATEMENT;
    node->as.while_statement.condition = condition;
    node->as.while_statement.body = body;
    return node;
}

ASTNode* new_for_statement_node(ASTNode* initializer, ASTNode* condition, ASTNode* increment, ASTNode* body) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FOR_STATEMENT;
    node->as.for_statement.initializer = initializer;
    node->as.for_statement.condition = condition;
    node->as.for_statement.increment = increment;
    node->as.for_statement.body = body;
    return node;
}

ASTNode* new_function_node(Token name, uint8_t arity, FunctionType type, ASTNode* body) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FUNCTION;
    node->as.function.name = name;
    node->as.function.arity = arity;
    node->as.function.type = type;
    node->as.function.body = body;
    return node;
}

ASTNode* new_string_node(char* value) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_STRING;
    node->as.string.string = value;
    return node;
}

ASTNode* new_super_call_node(Token name, ASTNode** arguments) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_SUPER_METHOD_CALL;
    node->as.superMethodCall.name = name;
    node->as.superMethodCall.arguments = arguments;
    return node;
}

ASTNode* new_super_property_access_node(Token name) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_SUPER_PROPERTY_ACCESS;
    node->as.superPropertyAccess.name = name;
    return node;
}

ASTNode* new_continue_statement_node(int offset) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_CONTINUE_STATEMENT;
    node->as.continue_statement.offset = offset;
    return node;
}

ASTNode* new_break_statement_node() {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BREAK_STATEMENT;
    return node;
}

ASTNode* new_variable_assignment_node(Token name, VariableAccessType accessType, int arg, ASTNode* value) {
    printf("??? ------\n");
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    node->type = NODE_VARIABLE_ASSIGNMENT;
    printf("wtf ------\n");
    print_ast_node(node, 0);
    node->as.variableAssignment.name = name;
    node->as.variableAssignment.accessType = accessType;
    node->as.variableAssignment.arg = arg;
    node->as.variableAssignment.value = value;
    
    return node;
}

ASTNode* new_variable_access_node(Token name, VariableAccessType accessType, int arg) {
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    node->type = NODE_VARIABLE_ACCESS;
    node->as.variableAccess.name = name;
    node->as.variableAccess.accessType = accessType;
    node->as.variableAccess.arg = arg;
    return node;
}

const char* const token_type_to_string[] = {
  // Single-character tokens.
  "TOKEN_LEFT_PAREN", "TOKEN_RIGHT_PAREN",
  "TOKEN_LEFT_BRACE", "TOKEN_RIGHT_BRACE",
  "TOKEN_COMMA", "TOKEN_DOT", "TOKEN_MINUS", "TOKEN_PLUS",
  "TOKEN_SEMICOLON", "TOKEN_SLASH", "TOKEN_STAR",
  // One or two character tokens.
  "TOKEN_BANG", "TOKEN_BANG_EQUAL",
  "TOKEN_EQUAL", "TOKEN_EQUAL_EQUAL",
  "TOKEN_GREATER", "TOKEN_GREATER_EQUAL",
  "TOKEN_LESS", "TOKEN_LESS_EQUAL",
  // Literals.
  "TOKEN_IDENTIFIER", "TOKEN_STRING", "TOKEN_NUMBER",
  // Keywords.
  "TOKEN_AND", "TOKEN_CLASS", "TOKEN_ELSE", "TOKEN_FALSE",
  "TOKEN_FOR", "TOKEN_FUN", "TOKEN_IF", "TOKEN_NIL", "TOKEN_OR",
  "TOKEN_RETURN", "TOKEN_SUPER", "TOKEN_THIS",
  "TOKEN_TRUE", "TOKEN_VAR", "TOKEN_WHILE", "TOKEN_BREAK",
  "TOKEN_CONTINUE",

  "TOKEN_ERROR", "TOKEN_EOF"
};

const char* const node_type_to_string[] = {
    "NODE_BINARY",
    "NODE_CALL",
    "NODE_GET_PROPERTY",
    "NODE_SET_PROPERTY",
    "NODE_INVOKE",
    "NODE_LITERAL",
    "NODE_NUMBER",
    "NODE_UNARY",
    "NODE_LOGICAL",
    "NODE_GROUPING",
    "NODE_VARIABLE",
    "NODE_ASSIGNMENT",
    "NODE_EXPRESSION_STATEMENT",
    "NODE_IF_STATEMENT",
    "NODE_WHILE_STATEMENT",
    "NODE_FOR_STATEMENT",
    "NODE_FUNCTION",
    "NODE_RETURN_STATEMENT",
    "NODE_CLASS_DECLARATION",
    "NODE_BLOCK",
    "NODE_SUPER_PROPERTY_ACCESS",
    "NODE_SUPER_METHOD_CALL",
    "NODE_STRING",
    "NODE_CONTINUE_STATEMENT",
    "NODE_BREAK_STATEMENT",
    "NODE_VARIABLE_ASSIGNMENT",
    "NODE_VARIABLE_ACCESS",
};

// Helper function to print indentation
static void print_indent(int depth) {
    for (int i = 0; i < depth; i++) {
        printf("  ");
    }
}


void print_ast_node(ASTNode* node, int depth) {
    if (!node) return;
    print_indent(depth);
    printf("%s", node_type_to_string[node->type]);

    switch (node->type) {
        case NODE_BINARY:
            printf(" (operator: %s)\n", token_type_to_string[node->as.binary.operator]);
            print_ast_node(node->as.binary.left, depth + 1);
            print_ast_node(node->as.binary.right, depth + 1);
            break;
        case NODE_CALL:
            printf("\n");
            print_ast_node(node->as.call.callee, depth + 1);
            for (int i = 0; i < node->as.call.arg_count; i++) {
                print_ast_node(node->as.call.arguments[i], depth + 1);
            }
            break;
        // Add more cases for other node types here
        // ...
        default:
            printf("\n");
            break;
    }
}

void print_ast(ASTNode* root) {
    print_ast_node(root, 0);
}
