#include <stdbool.h>
#include <stdio.h>

#include "list.h"
#include "ast.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

ASTNode* new_binary_node(TokenType operator, ASTNode* left, ASTNode* right) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_BINARY;
    node->as.binary.operator = operator;
    node->as.binary.left = left;
    node->as.binary.right = right;
    return node;
}

ASTNode* new_call_node(ASTNode* callee) {
    printf("new call node");
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_CALL;
    node->as.call.callee = callee;
    node->as.call.arguments = malloc(sizeof(List));
    initList(node->as.call.arguments);
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

ASTNode* new_invoke_node(ASTNode* object, Token name) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_INVOKE;
    node->as.invoke.object = object;
    node->as.invoke.name = name;
    node->as.invoke.arguments = malloc(sizeof(List));
    initList(node->as.invoke.arguments);
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

ASTNode* new_super_call_node(Token name) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_SUPER_METHOD_CALL;
    node->as.superMethodCall.name = name;
    node->as.superMethodCall.arguments = malloc(sizeof(List));
    initList(node->as.superMethodCall.arguments);
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
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    node->type = NODE_VARIABLE_ASSIGNMENT;
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
            printf(" (operator: %s)\n", tokenTypeToString(node->as.binary.operator));
            print_ast_node(node->as.binary.left, depth + 1);
            print_ast_node(node->as.binary.right, depth + 1);
            break;
        case NODE_CALL:
            printf("\n");
            printf("\tCallee: ");
            print_ast_node(node->as.call.callee, depth);
            for (int i = 0; i < node->as.call.arguments->count; i++) {
                printf("\tArg %d: ", i);
                print_ast_node((ASTNode*)node->as.call.arguments->values[i], depth);
            }
            break;
        case NODE_NUMBER:
            printf(" (value: %f)\n", node->as.number.value);
            break;
        case NODE_STRING:
            printf(" (value: %s)\n", node->as.string.string);
            break;
        case NODE_VARIABLE_ACCESS:
            printf(" (name: %s)\n", node->as.variableAccess.name.start);
            break;
        case NODE_VARIABLE_ASSIGNMENT:
            printf(" (name: %s)\n", node->as.variableAssignment.name.start);
            print_ast_node(node->as.variableAssignment.value, depth + 1);
            break;
        default:
            printf("\n");
            break;
    }
}

void print_ast(ASTNode* root) {
    print_ast_node(root, 0);
}
