#ifndef clox_ast_h
#define clox_ast_h

#include <stdlib.h>

#include "list.h"
#include "compiler.h"
#include "scanner.h"

typedef struct ASTNode ASTNode;

typedef enum {
    NODE_BINARY,
    NODE_CALL,
    NODE_GET_PROPERTY,
    NODE_SET_PROPERTY,
    NODE_INVOKE,
    NODE_LITERAL,
    NODE_NUMBER,
    NODE_UNARY,
    NODE_LOGICAL,
    NODE_GROUPING,
    NODE_VARIABLE,
    NODE_ASSIGNMENT,
    NODE_EXPRESSION_STATEMENT,
    NODE_IF_STATEMENT,
    NODE_VAR_DECLARATION,
    NODE_WHILE_STATEMENT,
    NODE_FOR_STATEMENT,
    NODE_FUNCTION,
    NODE_RETURN_STATEMENT,
    NODE_CLASS_DECLARATION,
    NODE_BLOCK,
    NODE_SUPER_PROPERTY_ACCESS,
    NODE_SUPER_METHOD_CALL,
    NODE_STRING,
    NODE_CONTINUE_STATEMENT,
    NODE_BREAK_STATEMENT,
    NODE_VARIABLE_ASSIGNMENT,
    NODE_VARIABLE_ACCESS,
} NodeType;

typedef struct {
    Token className;
    Token superclass;
    bool hasSuperclass;
    List methods; // List of ASTNode*, each representing a method
} ClassDeclarationNodeData;


typedef enum {
    ACCESS_LOCAL,
    ACCESS_UPVALUE,
    ACCESS_GLOBAL
} VariableAccessType;

typedef struct {
    Token name;
    VariableAccessType accessType;
    int arg;
    ASTNode* value;
} VariableAssignmentNodeData;

typedef struct {
    Token name;
    VariableAccessType accessType;
    int arg;
} VariableAccessNodeData;

typedef struct {
    Token name;
    ASTNode** arguments;
} SuperMethodCallNodeData;

typedef struct {
    Token name;
} SuperPropertyAccessNodeData;

struct ASTNode {
    NodeType type;
    union {
        struct {
            ASTNode* left;
            ASTNode* right;
            TokenType operator;
        } binary;
        struct {
            ASTNode* callee;
            ASTNode** arguments;
            int arg_count;
        } call;
        struct {
            TokenType token_type;
        } literal;
        struct {
            double value;
        } number;
        struct {
            TokenType operator;
            ASTNode* operand;
        } unary;
        struct {
            ASTNode* object;
            uint8_t name;
        } get_property;
        struct {
            ASTNode* object;
            uint8_t name;
            ASTNode* value;
        } set_property;
        struct {
            ASTNode* object;
            uint8_t name;
            ASTNode** arguments;
            int arg_count;
        } invoke;
        struct {
            ASTNode* left;
            ASTNode* right;
            TokenType operator;
        } logical;
        struct {
            ASTNode* expression;
        } grouping;
        struct {
            bool is_local;
            int index;
        } variable;
        struct {
            ASTNode* value;
            bool is_local;
            int index;
        } assignment;
        struct {
            ASTNode* expression;
        } expression_statement;
        struct {
            ASTNode* condition;
            ASTNode* then_branch;
            ASTNode* else_branch;
        } if_statement;
        struct {
            ASTNode* initializer;
            uint8_t name;
        } var_declaration;
        struct {
            ASTNode* condition;
            ASTNode* body;
        } while_statement;
        struct {
            ASTNode* initializer;
            ASTNode* condition;
            ASTNode* increment;
            ASTNode* body;
        } for_statement;
        struct {
            Token name;
            uint8_t arity;
            FunctionType type;
            ASTNode* body;
        } function;
        struct {
            ASTNode* value;
        } return_statement;
        struct {
            ASTNode** statements;
            int statement_count;
        } block;
        ClassDeclarationNodeData classDeclaration;
        VariableAssignmentNodeData variableAssignment;
        VariableAccessNodeData variableAccess;
        SuperMethodCallNodeData superMethodCall;
        SuperPropertyAccessNodeData superPropertyAccess;
        struct {
            char* string;
        } string;
        struct {
            int offset;
        } continue_statement;
        struct {} break_statement;
    } as;
};

// Functions to create ASTNode instances
ASTNode* new_binary_node(TokenType operator, ASTNode* left, ASTNode* right);
ASTNode* new_call_node(ASTNode* callee, ASTNode** arguments, int arg_count);
ASTNode* new_get_property_node(ASTNode* object, uint8_t name);
ASTNode* new_set_property_node(ASTNode* object, uint8_t name, ASTNode* value);
ASTNode* new_invoke_node(ASTNode* object, uint8_t name, ASTNode** arguments, int arg_count);
ASTNode* new_literal_node(TokenType token_type);
ASTNode* new_number_node(double value);
ASTNode* new_unary_node(TokenType operator, ASTNode* operand);
ASTNode* new_logical_node(TokenType operator, ASTNode* left, ASTNode* right);
ASTNode* new_grouping_node(ASTNode* expression);
ASTNode* new_expression_statement_node(ASTNode* expression);
ASTNode* new_if_statement_node(ASTNode* condition, ASTNode* then_branch, ASTNode* else_branch);
ASTNode* new_while_statement_node(ASTNode* condition, ASTNode* body);
ASTNode* new_for_statement_node(ASTNode* initializer, ASTNode* condition, ASTNode* increment, ASTNode* body);
ASTNode* new_function_node(Token name, uint8_t arity, FunctionType type, ASTNode* body);
ASTNode* new_return_statement_node(ASTNode* value);
ASTNode* new_class_declaration_node(Token name, Token superclass, bool hasSuperclass, List methods);
ASTNode* new_block_node();
ASTNode* new_super_property_access_node(Token name);
ASTNode* new_super_call_node(Token name, ASTNode** arguments);
ASTNode* new_string_node(char* string);
ASTNode* new_continue_statement_node(int offset);
ASTNode* new_break_statement_node();
ASTNode* new_variable_assignment_node(Token name, VariableAccessType accessType, int arg, ASTNode* value);
ASTNode* new_variable_access_node(Token name, VariableAccessType accessType, int arg);



#endif