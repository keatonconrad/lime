#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"
#include "list.h"

typedef struct {
    const char* start; // Beginning of current lexeme being scanned
    const char* current; // Current character being looked at
    int line; // What line current lexeme is on
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
            c == '_';
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

static bool isAtEnd() {
    return *scanner.current == '\0';
}

static char advance() {
    scanner.current++;
    return scanner.current[-1];
}

static char peek() {
    return *scanner.current;
}

static char peekNext() {
    // Returns the character one past the current one
    if (isAtEnd()) return '\0';
    return scanner.current[1];
}

static bool match(char expected) {
    if (isAtEnd()) return false;
    if (*scanner.current != expected) return false;
    scanner.current++;
    return true;
}

static Token makeToken(TokenType type) {
    Token token;
    token.type = type;
    token.start = scanner.start;
    token.length = (int)(scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

static Token errorToken(const char* message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = scanner.line;
    return token;
}

static void skipWhitespace() {
    for (;;) {
        char c = peek();
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '\n':
                scanner.line++;
                advance();
                break;
            case '/':
                if (peekNext() == '/') {
                    // A comment goes until the end of the line
                    while (peek() != '\n' && !isAtEnd()) advance();
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

static TokenType checkKeyword(int start, int length, const char* rest, TokenType type) {
    if (scanner.current - scanner.start == start + length &&
        memcmp(scanner.start + start, rest, length) == 0) {
        return type;
    }
    
    return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
    switch (scanner.start[0]) {
        case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
        case 'b': return checkKeyword(1, 4, "reak", TOKEN_BREAK);
        case 'c': 
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'l': return checkKeyword(2, 3, "ass", TOKEN_CLASS);
                    case 'o': return checkKeyword(2, 6, "ntinue", TOKEN_CONTINUE);
                }
            }
            break;
        case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
        case 'f':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
                    case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
                    case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN);
                }
            }
            break;
        case 'i': return checkKeyword(1, 1, "f", TOKEN_IF);
        case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL);
        case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
        case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
        case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER);
        case 't':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS);
                    case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
                }
            }
            break;
        case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
        case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
    }

    return TOKEN_IDENTIFIER;
}

static Token identifier() {
    while (isAlpha(peek()) || isDigit(peek())) advance();
    return makeToken(identifierType());
}

static Token number() {
    while (isDigit(peek())) advance();

    // Look for a fractional part
    if (peek() == '.' && isDigit(peekNext())) {
        // Consume the "."
        advance();

        while (isDigit(peek())) advance();
    }
    
    return makeToken(TOKEN_NUMBER);
}

static Token string() {
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') scanner.line++;
        advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    advance(); // The closing quote
    return makeToken(TOKEN_STRING);
}

Token scanToken() {
    skipWhitespace();
    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(TOKEN_EOF);

    char c = advance();
    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();

    switch (c) {
        case '(': return makeToken(TOKEN_LEFT_PAREN);
        case ')': return makeToken(TOKEN_RIGHT_PAREN);
        case '{': return makeToken(TOKEN_LEFT_BRACE);
        case '}': return makeToken(TOKEN_RIGHT_BRACE);
        case ';': return makeToken(TOKEN_SEMICOLON);
        case ',': return makeToken(TOKEN_COMMA);
        case '.': return makeToken(TOKEN_DOT);
        case '-': return makeToken(TOKEN_MINUS);
        case '+': return makeToken(TOKEN_PLUS);
        case '/': return makeToken(TOKEN_SLASH);
        case '*': return makeToken(TOKEN_STAR);
        case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '"': return string();
    }

    return errorToken("Unexpected character.");
}

static const char* const type_to_string[] = {
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

const char* const tokenTypeToString(TokenType type) {
    size_t num_types = sizeof(type_to_string) / sizeof(type_to_string[0]);
    if (type < 0 || type >= num_types) {
        printf("Error: Invalid TokenType value: %d\n", type);
        return "TOKEN_ERROR";
    }
    return type_to_string[type];
}


void scanTokens(const char* source, List* tokens) {
    initScanner(source);

    Token* token;
    do {
        token = (Token*)malloc(sizeof(Token));
        if (token == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(1);
        }
        *token = scanToken();
        writeList(tokens, token);
    } while (token->type != TOKEN_EOF);
}


void printTokens(List* tokens) {
    printf("Token count: %d\n", tokens->count);
    for (int i = 0; i < tokens->count; i++) {
        Token* token = (Token*)listGet(tokens, i);
        printf("%s, ", tokenTypeToString(token->type));
    }
}

void printToken(Token* token) {
    printf("%s, ", tokenTypeToString(token->type));
    char identifier[256]; // Make sure this buffer is large enough for your identifiers.
    memcpy(identifier, token->start, token->length);
    identifier[token->length] = '\0';
    printf("%s", identifier);
}