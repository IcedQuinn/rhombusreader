import options

type
   TokenKind* = enum
      ## Determines what type of token is being emitted.
      tkError
      tkEOF
      tkComment
      tkWhitespace
      tkStringFragment
      tkStringEscape
      tkBinary
      tkInteger
      tkPercent
      tkHash
      tkDollar
      tkOpenBrace
      tkCloseBrace
      tkOpenParenthesis
      tkCloseParenthesis
      tkOpenBracket
      tkCloseBracket
      tkOpenAngle
      tkCloseAngle
      tkIdentifier
      tkPathSeparator
      tkBang
      tkColon
      tkPeriod
      tkAt
      tkQuote
      tkSingleQuote

   Token* = object
      ## Holds the kind of token and any extra details about it.
      case kind: TokenKind
      of tkStringFragment, tkStringEscape, tkIdentifier, tkBinary, tkComment:
         sdata*: string
      of tkInteger:
         idata*: int
      of tkWhitespace:
         lines*: int
      else: discard

func is_ident_leader(ch: char): bool =
   return ch in 'a'..'z' or ch in 'A'..'Z' or ch in '*'..'-' or ch in '<'..'?' or ch == '`' or ch == '|' or ch == '~'

func is_ident(ch: char): bool =
   return is_ident_leader(ch) or ch in '0'..'9'

func is_binary(ch: char): bool =
   return ch in '0'..'9' or ch in 'a'..'z' or ch in 'A'..'Z' or ch == '='

func is_digit(ch: char): bool =
   return ch in '0'..'9'

func is_whitespace(ch: char): bool =
   return ch.int in 0..32

func read_ident(source: string; start: var int): Option[string] =
   ## Read a series of identifier characters starting with a leader.
   if start notin 0..source.high: return none[string]()
   var here = start
   if not is_ident_leader(source[here]): return none[string]()
   while is_ident(source[here]): inc here
   result = some[string](source.substr(start, here-1))
   start = here

func read_strfrag(source: string; start: var int): Option[string] =
   ## Read a series of identifier characters starting with a leader.
   if start notin 0..source.high: return none[string]()
   var here = start
   while here in 0..source.high:
      if source[here] == '\"' or source[here] == '^':
         start = here
         return some[string](source.substr(start, here-1))
      inc here

func read_strescape(source: string; start: var int): Option[string] =
   ## Read a series of identifier characters starting with a leader.
   if start notin 0..source.high: return none[string]()
   var here = start
   if source[here] != '^': return none[string]()
   inc here
   if source[here] != '(': return none[string]()
   inc here

   var k = here
   while here in 0..source.high:
      if source[here] == ')':
         start = here
         return some[string](source.substr(k, here-1))
      inc here

func read_numeric(source: string; start: var int; integer: var Option[int]; binary: var Option[string]) =
   ## Read a series of digits and interpret them as an integer.
   ## Apostrophes inside an integer are ignored.
   if start notin 0..source.high: return
   var here = start
   var negative = false
   var accum = 0

   if source[here] == '-':
      negative = true
   elif not is_digit(source[here]):
      return

   # first try to read as an integer and understand it directly; if that
   # fails we bail out and a second attempt tries to read the rest of the
   # data as an encoded binary blob.
   block read_as_int:
      while here in 0..source.high:
         if source[here] == '\'': inc here
         if not is_digit(source[here]): break read_as_int
         accum *= 10
         accum += source[here].int - '0'.int
         inc here

      if negative: accum *= -1

      integer = some[int](accum)
      start = here
      return

   # if we have not ejected by now we have to treat this as a binary
   # chunk. that can't be understood until a later parsing step.
   while here in 0..source.high:
      if source[here] == '\'': inc here
      if not is_binary(source[here]): break
      inc here

   binary = some[string](source.substr(start, here-1))
   start = here

func decimalize(value: int): float =
   ## Count the number of place values in the integer.
   result = value.float
   while result >= 0:
      result *= 0.1

func assemble_float(leading, trailing: Option[int]): Option[float] =
   if leading.is_none or trailing.is_none: return none[float]()
   return some[float](leading.get.float + trailing.get.decimalize)

func read_whitespace(source: string; start: var int): Option[int] =
   if start notin 0..source.high: return
   var here = start
   var lines = 0
   while is_whitespace(source[here]):
      if source[here] == '\n': inc lines
      inc here
   if start == here: return
   start = here
   return some[int](lines)

func read_comment(source: string; start: var int): Option[string] =
   if start notin 0..source.high: return
   var here = start
   if source[here] != ';': return
   inc here
   let open = here
   while here in 0..source.high:
      if source[here] == '\n': break
      inc here
   let close = here-1
   start = here
   return some[string](source.substr(open, close))

# TODO allow long strings inside nested {}'s
# TODO delimited strings like {foo{...}foo}

iterator lexer*(source: string; here: var int): Token =
   var output: Token
   var last = tkError
   while here in 0..source.high:
      block figure_shit_out:
         let whitespace = read_whitespace(source, here)
         if whitespace.is_some:
            output = Token(kind: tkWhitespace, lines: whitespace.get)
            break figure_shit_out

         # strings are a bit bizzare to deal with
         case last
         of tkQuote, tkStringFragment, tkStringEscape:
            if source[here] == '"':
               output = Token(kind: tkQuote)
               break figure_shit_out
            elif source[here] == '^':
               let escaped = read_strescape(source, here)
               if escaped.is_none:
                  output = Token(kind: tkError)
               else:
                  output = Token(kind: tkStringEscape, sdata: escaped.get)
            else:
               let frag = read_strfrag(source, here)
               if frag.is_none:
                  output = Token(kind: tkError)
               else:
                  output = Token(kind: tkStringFragment, sdata: frag.get)
         else:
            if source[here] == '"':
               output = Token(kind: tkQuote)
               break figure_shit_out

         # check for a plain old identifier
         var ident = read_ident(source, here)
         if ident.is_some:
            output = Token(kind: tkIdentifier, sdata: ident.get)
            break figure_shit_out

         # check for an integer or binary blob
         var integer: Option[int]
         var binary: Option[string]
         read_numeric(source, here, integer, binary)
         if integer.is_some:
            output = Token(kind: tkInteger, idata: integer.get)
            break figure_shit_out
         elif binary.is_some:
            output = Token(kind: tkBinary, sdata: binary.get)
            break figure_shit_out

         if source[here] == ';':
            let comment = read_comment(source, here)
            if comment.is_none:
               output = Token(kind: tkError)
            else:
               output = Token(kind: tkComment, sdata: comment.get)
            break figure_shit_out

         case source[here]
         of '%': output = Token(kind: tkPercent)
         of '#': output = Token(kind: tkHash)
         of '$': output = Token(kind: tkDollar)
         of '{': output = Token(kind: tkOpenBrace)
         of '}': output = Token(kind: tkCloseBrace)
         of '(': output = Token(kind: tkOpenParenthesis)
         of ')': output = Token(kind: tkCloseParenthesis)
         of '[': output = Token(kind: tkOpenBracket)
         of ']': output = Token(kind: tkCloseBracket)
         of '/': output = Token(kind: tkPathSeparator)
         of '!': output = Token(kind: tkBang)
         of ':': output = Token(kind: tkColon)
         of '.': output = Token(kind: tkPeriod)
         of '<': output = Token(kind: tkOpenAngle)
         of '>': output = Token(kind: tkCloseAngle)
         of '@': output = Token(kind: tkAt)
         of '\'': output = Token(kind: tkSingleQuote)
         else:
            output = Token(kind: tkError)
            break figure_shit_out

         inc here 

      last = output.kind
      yield output
      if last == tkError: break

var code = "author: @icedquinn@blob.cat ; henlo fediblobs\n out: sample2d texture uv/xy soup [44 22] jingle: 92 + 7"
var marker = 0
for token in lexer(code, marker):
   echo token

