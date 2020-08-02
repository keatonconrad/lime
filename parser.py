from rply import ParserGenerator
from ast import Sum, Sub, Print, Program, Integer, Double, Mul, Div, Mod
import collections


def flatten(list_):
    for el in list_:
        if isinstance(el, collections.Iterable) and not isinstance(el, (str, bytes)):
            yield from flatten(el)
        else:
            yield el


class Parser():
    def __init__(self, module, builder, printf):
        self.pg = ParserGenerator(
            # A list of all token names accepted by the parser.
            ['DOUBLE', 'INTEGER', 'STRING', 'BOOL', 'IF', 'ELSE', 'AND', 'OR',
             'NOT', 'VAR', 'FUNCTION', 'PRINT', 'LOOP', 'USING', 'IDENTIFIER',
             'DOUBLE_EQUAL', 'NOT_EQUAL', 'GTE', 'LTE', 'GT', 'LT', 'EQUAL',
             'LBRACKET', 'RBRACKET', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN',
             'PIPE', 'COMMA', 'SEMICOLON', 'DOT', 'COLON', 'PLUS', 'MINUS',
             'MUL', 'DIV', 'MOD'
             ],
            # A list of precedence rules with ascending precedence, to
            # disambiguate ambiguous production rules.
            precedence=[
                ('left', ['FUNCTION', 'PRINT', 'LOOP']),
                ('left', ['VAR', ]),
                ('left', ['EQUAL']),
                ('left', ['LBRACKET', 'RBRACKET', 'COMMA']),
                ('left', ['IF', 'COLON', 'ELSE', ]),
                ('left', ['AND', 'OR', ]),
                ('left', ['NOT', ]),
                ('left', ['DOUBLE_EQUAL', 'NOT_EQUAL', 'GTE', 'GT', 'LT', 'LTE', ]),
                ('left', ['PLUS', 'MINUS', ]),
                ('left', ['MUL', 'DIV', 'MOD']),
            ]
        )
        self.module = module
        self.builder = builder
        self.printf = printf

    def parse(self):
        @self.pg.production('program : statement_seq')
        def program(p):
            p = list(flatten(p))
            return Program(p)

        @self.pg.production('statement_seq : statement')
        @self.pg.production('statement_seq : statement statement_seq')
        def statement_seq(p):
            return p

        @self.pg.production('statement : PRINT LPAREN expression RPAREN SEMICOLON')
        def statement_print(p):
            return Print(self.builder, self.module, self.printf, p[2])

        @self.pg.production('expression : expression PLUS expression')
        @self.pg.production('expression : expression MINUS expression')
        @self.pg.production('expression : expression MUL expression')
        @self.pg.production('expression : expression DIV expression')
        @self.pg.production('expression : expression MOD expression')
        def expression(p):
            left = p[0]
            right = p[2]
            operator = p[1]
            if operator.gettokentype() == 'PLUS':
                return Sum(self.builder, self.module, left, right)
            elif operator.gettokentype() == 'MINUS':
                return Sub(self.builder, self.module, left, right)
            elif operator.gettokentype() == 'MUL':
                return Mul(self.builder, self.module, left, right)
            elif operator.gettokentype() == 'DIV':
                return Div(self.builder, self.module, left, right)
            elif operator.gettokentype() == 'MOD':
                return Mod(self.builder, self.module, left, right)

        @self.pg.production('expression : INTEGER')
        def exp_integer(p):
            return Integer(self.builder, self.module, p[0].value)

        @self.pg.production('expression : DOUBLE')
        def exp_double(p):
            print(p)
            return Double(self.builder, self.module, p[0].value)

        @self.pg.production('expression : BOOL')
        def exp_bool(p):
            if p[0].value == 'false':
                return Integer(self.builder, self.module, 0)
            else:
                return Integer(self.builder, self.module, 1)

        @self.pg.error
        def error_handle(token):
            raise ValueError(token)

    def get_parser(self):
        return self.pg.build()
