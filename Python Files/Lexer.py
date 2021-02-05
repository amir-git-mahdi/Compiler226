from Position import *
from Error import *
from Token import *
import string

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'
LETTERS = string.ascii_letters.__add__('&')
LETTERS_DIGITS = LETTERS + DIGITS
# all letters!
upper_letter = LETTERS.upper()
lower_letter = LETTERS.lower()

# removing the duplicates
big_letter = "".join(set(upper_letter)).replace('&', '')
low_letter = "".join(set(lower_letter))


C_INT = 'INT'
C_POWER = 'LSEMI'
C_FLOAT = 'FLOAT'
C_STRING = 'STRING'
C_IDENTIFIER = 'IDENTIFIER'
C_KEYWORD = 'KEYWORD'
C_PLUS = 'PLUS'
C_MINUS = 'MINUS'
C_ADDONE = 'ADDONE'
C_MINONE = 'MINONE'
C_REMAIN = 'REMAIN'

C_MUL = 'MUL'
C_DIV = 'DIV'
C_POW = 'POW'

# equals
C_EQ = 'EQ'

# logical operations

C_EE = 'EE'
C_NE = 'NE'
C_LT = 'LT'
C_GT = 'GT'
C_LTE = 'LTE'
C_GTE = 'GTE'

C_LPAREN = 'LPAREN'
C_RPAREN = 'RPAREN'
C_COMMA = 'COMMA'
C_ARROW = 'ARROW'
C_EOF = 'EOF'


KEYWORDS = [
    'Harf',
    'Sahih',
    'Ashari',
    'VAR',
    'Agar',
    'ELSE',
    'Ta',
    'FUN',
    'THEN',
    'Benevis',
]


class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    def make_tokens(self):
        # In this method we are checking [(, ", )] and if it is in our LETTERS!
        # (tokens = []) is an empty list that we append type and value to this list!
        tokens = []

        while self.current_char != None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char in LETTERS:
                tokens.append(self.m_stringToc())
            elif self.current_char == '"':
                tokens.append(self.make_string())
#            elif self.current_char in big_letter:
#                tokens.append(self.make_identifier())
            # Keywords are all upper case
            # we use this to split them from the other token

            elif self.current_char == '=':
                tokens.append(self.make_equals())
            elif self.current_char == '^':
                tokens.append(Token(C_POW, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(self.make_minus_or_arrow())
            elif self.current_char == '(':
                tokens.append(Token(C_LPAREN, pos_start=self.pos))
                self.advance()

            elif self.current_char == ')':
                tokens.append(Token(C_RPAREN, pos_start=self.pos))
                self.advance()

            elif self.current_char == ',':
                tokens.append(Token(C_COMMA, pos_start=self.pos))
                self.advance()
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        tokens.append(Token(C_EOF, pos_start=self.pos))
        return tokens, None
    def m_stringToc(self):
        # in check() method we are checking if a number is after the string token!
        def checkNum():
            if self.current_char in DIGITS:
                return False

        # defining an empty string to store char by char (temp.__add__) using __add__ method!
        temp = ''
        while self.current_char!= None and self.current_char not in [' ','\t'] and self.current_char != '(' and self.current_char != ')' and self.current_char != '"'  and checkNum() != False:
            temp = temp.__add__(self.current_char)
            self.advance()

            #self.m_tokens()
        if temp == 'Jam':
            return Token(C_PLUS)
        elif temp == 'Kam':
            return Token(C_MINUS)
        elif temp == 'Equals':
            return Token(C_EQ)
        elif temp == 'Zarb':
            return Token(C_MUL)
        elif temp == 'Tagsim':
            return Token(C_DIV)
        elif temp == 'Bagimonde':
            return Token(C_REMAIN)
        elif temp == 'Yekibala':
            return Token(C_ADDONE)
        elif temp == 'yekipaein':
            return Token(C_MINONE)

        # Maybe we define a method for making logical tokens as m_logicalToc()!
        elif temp == '&BM':
            return Token(C_GTE)
        elif temp == '&KM':
            return Token(C_LTE)
        elif temp == '&MM':
            return Token(C_EE)
        elif temp == '&B':
            return Token(C_GT)
        elif temp == '&K':
            return Token(C_LT)

        elif temp in KEYWORDS:
            pos_start = self.pos.copy()
            return Token(C_KEYWORD, temp, pos_start, self.pos)

        else:
            pos_start = self.pos.copy()
            return Token(C_IDENTIFIER, temp, pos_start, self.pos)


#        elif temp == 'begir':
#            return Token(C_INPUT)

#        else:
#            raise NameError(f"'{temp}' not defined ")  # Raise Error

    # make int and fload number
    # casting the string number to int or float number
    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()
        # find out whether  str number is int or float
        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                # as soon a we find a single dot(.) , we mark the number as float
                if dot_count == 1:
                    break
                dot_count += 1
            num_str += self.current_char
            self.advance()
        # float case
        if dot_count == 0:
            return Token(C_INT, int(num_str), pos_start, self.pos)
        # int case
        else:
            return Token(C_FLOAT, float(num_str), pos_start, self.pos)

    def make_string(self):
        string = ''
        pos_start = self.pos.copy()
        jump_char = False
        self.advance()

        jump_chars = {
            'n': '\n',
            't': '\t'
        }

        while self.current_char != None and (self.current_char != '"' or jump_char):
            if jump_char:
                string += jump_chars.get(self.current_char, self.current_char)
            else:
                if self.current_char == '\\':
                    jump_char = True
                else:
                    string += self.current_char
            self.advance()
            jump_char = False

        self.advance()
        return Token(C_STRING, string, pos_start, self.pos)


    '''def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()

        while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.advance()

        tok_type = C_KEYWORD if id_str in KEYWORDS else C_IDENTIFIER
        return Token(tok_type, id_str, pos_start, self.pos)'''

    def make_equals(self):
        tok_type = C_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = C_EE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    def make_minus_or_arrow(self):
        tok_type = C_MINUS
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '>':
            self.advance()
            tok_type = C_ARROW

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
