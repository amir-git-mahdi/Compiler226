from sly import Lexer, Parser
class CalcLexer(Lexer):

    # Set of token names.

    tokens = { PLUS, MINUS, SAHIH }

    literals = { '(', ')'}

    # String containing ignored characters
    ignore = ' \t'

    # Regular expression rules
    PLUS    = r'JAM'
    MINUS = r'KAM'


    @_(r'\d+')
    def SAHIH(self, t):
        t.value = int(t.value)
        return t

    ignore_comment = r'\#.*'

    # Line number tracking

    def error(self, t):
        print('Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1


class CalcParser(Parser):
    # Get the token list from the lexer (required)
    tokens = CalcLexer.tokens

    @_('expr PLUS term')
    def expr(self, p):
        return p.expr + p.term

    @_('expr MINUS term')
    def expr(self, p):
        return p.expr - p.term


    @_('term')
    def expr(self, p):
        return p.term

    @_('factor')
    def term(self, p):
        return p.factor


    @_('SAHIH')
    def factor(self, p):
        return p.SAHIH


if __name__ == '__main__':
    lexer = CalcLexer()
    parser = CalcParser()

    while True:
        try:
            text = input('Calculate > ')
            result = parser.parse(lexer.tokenize(text))
            print(result)
        except EOFError:
            break
