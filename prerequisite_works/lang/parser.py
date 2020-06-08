from rply import ParserGenerator
from ast import Number, Plus, Minus, Multiply, Divide, Print

class Parser():
    def __init__(self, module, builder, printf):
        self.pg = ParserGenerator(
            # A list of all token names accepted by the parser.
            ['NUMBER', 'PRINT', 'OPEN_PAREN', 'CLOSE_PAREN',
             'SEMI_COLON', 'PLUS', 'MINUS', 'DIVIDE', 'MULTIPLY'],
             precedence=[ ('left', ['PLUS', 'MINUS']), ('left', ['MULTIPLY', 'DIVIDE']) ]
        )
        self.module = module
        self.builder = builder
        self.printf = printf

    def parse(self):
        @self.pg.production('program : PRINT OPEN_PAREN expression CLOSE_PAREN SEMI_COLON')
        def program(p):
            return Print(self.builder, self.module, self.printf, p[2])

        @self.pg.production('expression : expression PLUS expression')
        @self.pg.production('expression : expression MINUS expression')
        @self.pg.production('expression : expression DIVIDE expression')
        @self.pg.production('expression : expression MULTIPLY expression')
        def expression(p):
            left = p[0]
            right = p[2]
            operator = p[1]
            if operator.gettokentype() == 'PLUS':
                return Plus(self.builder, self.module, left, right)
            elif operator.gettokentype() == 'MINUS':
                return Minus(self.builder, self.module, left, right)
            elif operator.gettokentype() == 'DIVIDE':
                return Divide(self.builder, self.module, left, right)
            elif operator.gettokentype() == 'MULTIPLY':
                return Multiply(self.builder, self.module, left, right)

        @self.pg.production('expression : NUMBER')
        def number(p):
            return Number(self.builder, self.module, p[0].value)

        @self.pg.error
        def error_handle(token):
            raise ValueError(token)

    def get_parser(self):
        return self.pg.build()

