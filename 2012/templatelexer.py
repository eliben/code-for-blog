#-------------------------------------------------------------------------------
# templatelexer.py
#
# A lexer based on sub-generators. Requires Python 3.3+ to run.
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
from collections import namedtuple
import string


TOK_TEXT        = 'TOK_TEXT'
TOK_LEFT_META   = 'TOK_LEFT_META'
TOK_RIGHT_META  = 'TOK_RIGHT_META'
TOK_PIPE        = 'TOK_PIPE'
TOK_NUMBER      = 'TOK_NUMBER'
TOK_ID          = 'TOK_ID'


# A token has
#   type: one of the TOK_* constants
#   value: string value, as taken from input
#
Token = namedtuple('Token', 'type value')


class LexerError(Exception): pass


class TemplateLexer(object):
    """ A lexer for the template language. Initialize with the input
        string, and then call lex() which generates tokens. None is
        generated at EOF (and the generator expires).
    """
    def __init__(self, input):
        self.input = input
        # self.pos points at the current character in the input string
        self.pos = 0
        # self.tokstart points at the start of the currently processed
        # token
        self.tokstart = 0
        self.state = self._lex_text

    def lex(self):
        # self.state is one of the _lex_* state functions. Each such
        # function yields the tokens it finds and then returns the next
        # state function. When EOF is encountered, None is returned as
        # the new state.
        while self.state:
            self.state = yield from self.state()

    #--------- Internal ---------#

    _LEFT_META = '{{'
    _RIGHT_META = '}}'
    _PIPE = '|'
    _DOTS = set('.')
    _SIGNS = set('+-')
    _DEC_DIGITS = set(string.digits)
    _NUM_STARTERS = _SIGNS | _DEC_DIGITS
    _HEX_DIGITS = set(string.hexdigits)
    _ALPHANUM = _DEC_DIGITS | set(string.ascii_letters) | set('_')
    _EXP = set('eE')

    def _curchar(self):
        """ Get the char at self.pos, or None if we're at EOF.
        """
        return self.input[self.pos] if self.pos < len(self.input) else None

    def _accept(self, validset):
        """ Consume current char if it's in validset, and return True.
            Otherwise don't consume it, and return False.
        """
        if self._curchar() in validset:
            self.pos += 1
            return True
        else:
            return False

    def _accept_run(self, validset):
        """ Consume chars as long as they're in validset (or until EOF).
        """
        while self._accept(validset):
            pass
        
    def _ignore_tok(self):
        """ Ignore the current token.
        """
        self.tokstart = self.pos

    def _emit(self, toktype):
        """ Emit the current token
        """
        tok = Token(toktype, self.input[self.tokstart:self.pos])
        self.tokstart = self.pos
        return tok

    def _lex_text(self):
        # Look for the beginning of LEFT_META
        meta_start = self.input.find(self._LEFT_META, self.pos)
        if meta_start > 0:
            # Found. Emit all text until then (if any) and move to the
            # lex_left_meta state.
            self.pos = meta_start
            if self.pos > self.tokstart:
                yield self._emit(TOK_TEXT)
            return self._lex_left_meta
        else:
            # Not found. This means we're done. There may be some text
            # left until EOF, so emit it if there is.
            self.pos = len(self.input)
            if self.pos > self.tokstart:
                yield self._emit(TOK_TEXT)
            # Yield None to mark "no more tokens --> EOF"
            # Return None to stop the main lexing loop since there is no
            # next state.
            yield None
            return None
    
    def _lex_left_meta(self):
        self.pos += len(self._LEFT_META)
        yield self._emit(TOK_LEFT_META)
        return self._lex_inside_action

    def _lex_right_meta(self):
        self.pos += len(self._RIGHT_META)
        yield self._emit(TOK_RIGHT_META)
        return self._lex_text

    def _lex_inside_action(self):
        while True:
            # Check for RIGHT_META here before the next char is consumed,
            # to handle empty actions - {{}} - correctly.
            if self.input.startswith(self._RIGHT_META, self.pos):
                return self._lex_right_meta

            c = self._curchar()
            # Here a switch statement could be really useful...
            if c is None or c == '\n':
                raise LexerError('Unterminated action')
            elif c.isspace():
                self.pos += 1
                self._ignore_tok()
            elif c == self._PIPE:
                self.pos += 1
                yield self._emit(TOK_PIPE)
            elif c in self._NUM_STARTERS:
                return self._lex_number
            elif c.isalpha() or c == '_':
                return self._lex_identifier
            else:
                raise LexerError("Invalid char '%s' inside action")

        # Reached EOF
        raise LexerError('Unterminated action')
        return None

    def _lex_number(self):
        # Optional sign before the number
        self._accept(self._SIGNS)
        # Figure out if we'll have a decimal or hex number
        digits = self._DEC_DIGITS
        if self.input.startswith('0x', self.pos):
            self.pos += 2
            digits = self._HEX_DIGITS
        # Grab the number
        self._accept_run(digits)
        # It may be a float, followed by a dot and optionally more numbers
        if self._accept(self._DOTS):
            self._accept_run(digits)
        # It may be followed by an exponent
        if self._accept(self._EXP):
            self._accept(self._SIGNS)
            self._accept_run(self._DEC_DIGITS)
        yield self._emit(TOK_NUMBER)
        return self._lex_inside_action

    def _lex_identifier(self):
        self._accept_run(self._ALPHANUM)
        yield self._emit(TOK_ID)
        return self._lex_inside_action


#-------------------------------------------------------------------
if __name__ == '__main__':
    text = r'''
    sdfsdf
    Some t {{+45.e-12 0x23905.32 | }}{{ printf 2  |  |_2}}'''
    print('Lexing text:\n--------\n%s\n--------\n' % text)
    tlex = TemplateLexer(text)

    for t in tlex.lex():
        print(t)

