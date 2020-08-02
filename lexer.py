from rply import LexerGenerator


class Lexer():
    def __init__(self):
        self.lexer = LexerGenerator()

    def _add_tokens(self):
        self.lexer.add('DOUBLE', r'-?\d+\.\d+')
        self.lexer.add('INTEGER', r'-?\d+')
        self.lexer.add('STRING', r'(""".?""")|(".?")|(\'.?\')')
        self.lexer.add('BOOL', r"true(?!\w)|false(?!\w)")
        self.lexer.add('IF', r'if(?!\w)')
        self.lexer.add('ELSE', r'else(?!\w)')
        self.lexer.add('AND', r"&&")
        self.lexer.add('OR', r"\|\|")
        self.lexer.add('NOT', r"!")
        self.lexer.add('VAR', r'var(?!\w)')
        self.lexer.add('FUNCTION', r'func(?!\w)')
        self.lexer.add('PRINT', r'print(?!\w)')
        self.lexer.add('LOOP', r'loop(?!\w)')
        self.lexer.add('USING', r'using(?!\w)')
        self.lexer.add('IDENTIFIER', r'[a-zA-Z_]+[a-zA-Z0-9_]+')
        self.lexer.add('DOUBLE_EQUAL', r'==')
        self.lexer.add('NOT_EQUAL', r'!=')
        self.lexer.add('GTE', r'>=')
        self.lexer.add('LTE', r'<=')
        self.lexer.add('GT', r'>')
        self.lexer.add('LT', r'<')
        self.lexer.add('EQUAL', r'=')
        self.lexer.add('LBRACKET', r'\[')
        self.lexer.add('RBRACKET', r'\]')
        self.lexer.add('LBRACE', r'\{')
        self.lexer.add('RBRACE', r'\}')
        self.lexer.add('PIPE', r'\|')
        self.lexer.add('COMMA', r'\,')
        self.lexer.add('SEMICOLON', r';')
        self.lexer.add('DOT', r'\.')
        self.lexer.add('COLON', r'\:')
        self.lexer.add('PLUS', r'\+')
        self.lexer.add('MINUS', r'\-')
        self.lexer.add('MUL', r'\*')
        self.lexer.add('DIV', r'\/')
        self.lexer.add('MOD', r'\%')
        self.lexer.add('LPAREN', r'\(')
        self.lexer.add('RPAREN', r'\)')

        # ignore whitespace
        self.lexer.ignore('\s+')

    def get_lexer(self):
        self._add_tokens()
        return self.lexer.build()
