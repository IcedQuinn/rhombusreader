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
      tkMoney
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
      tkUrl
      tkPathSeparator
      tkBang
      tkColon
      tkPeriod
      tkX
      tkAt
      tkQuote
      tkSingleQuote

   Token* = object
      ## Holds the kind of token and any extra details about it.
      case kind*: TokenKind
      of tkStringFragment, tkStringEscape, tkIdentifier, tkBinary, tkComment, tkUrl:
         sdata*: string
      of tkInteger:
         idata*: int
      of tkWhitespace:
         lines*: int
      of tkMoney:
         name*: Option[string]
         amount*: int
      else: discard

func is_whitespace(ch: char): bool =
   return ch.int in 0..32

func is_digit(ch: char): bool =
   return ch in '0'..'9'

func is_ident_leader(ch: char): bool =
   return not (is_whitespace(ch) or is_digit(ch) or ch == ':' or ch == '@' or ch == '\'' or ch == '/' or ch == '%' or ch == '$' or ch == '#' or ch == '(' or ch == ')' or ch == '[' or ch == ']' or ch == '<' or ch == '>' or ch == '{' or ch == '}' or ch == '/')

func is_ident(ch: char): bool =
   return (not is_whitespace(ch)) and not (ch == '(' or ch == ')' or ch == '{' or ch == '}' or ch == '<' or ch == '>' or ch == '[' or ch == ']' or ch == '/')

func is_binary(ch: char): bool =
   return ch in '0'..'9' or ch in 'a'..'z' or ch in 'A'..'Z' or ch == '='

func read_ident(source: string; start: var int): Option[string] =
   ## Read a series of identifier characters starting with a leader.
   if start notin 0..source.high: return none[string]()
   var here = start
   if not is_ident_leader(source[here]): return none[string]()
   while here in 0..source.high:
      if not is_ident(source[here]): break
      inc here
   result = some[string](source.substr(start, here-1))
   start = here

func read_binary(source: string; start: var int): Option[string] =
   if start notin 0..source.high: return none[string]()
   var here = start
   if not is_binary(source[here]): return none[string]()
   while here in 0..source.high:
      if not is_binary(source[here]): break
      inc here
   result = some[string](source.substr(start, here-1))
   start = here

func read_anystring(source: string; start: var int): Option[string] =
   ## Read a series of identifier characters starting with a leader.
   if start notin 0..source.high: return
   var here = start
   while here in 0..source.high:
      if source[here].is_whitespace: break
      inc here
   if here != start:
      result = some[string](source.substr(start, here-1))
      start = here

func read_strfrag(source: string; start: var int): Option[string] =
   ## Read a series of identifier characters starting with a leader.
   if start notin 0..source.high: return none[string]()
   var here = start
   while here in 0..source.high:
      if source[here] == '\"' or source[here] == '^':
         if start != (here-1):
            result = some[string](source.substr(start, here-1))
         else:
            result = some[string]("")
         start = here
         return
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

func read_int(source: string; start: var int): Option[int] =
   ## Read a series of digits and interpret them as an integer.
   ## Apostrophes inside an integer are ignored.
   if start notin 0..source.high: return
   var here = start
   var negative = false
   var accum = 0

   if source[here] == '-':
      negative = true
      inc here
   elif source[here] == '+':
      inc here
   elif not is_digit(source[here]):
      return

   while here in 0..source.high:
      if source[here] == '\'':
         inc here
         continue
      if not is_digit(source[here]): break
      accum *= 10
      accum += source[here].int - '0'.int
      inc here

   if negative: accum *= -1

   result = some[int](accum)
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

func read_currency_name(source: string; start: var int): Option[string] =
   let valid = 0..source.high
   if start notin valid: return

   var here = start
   var lines = 0
   while not(is_whitespace(source[here])) and source[here] notin '0'..'9':
     inc here
   if start == here: return
   result = if here in valid:
         some[string](source.substr(start, here-1))
      else:
         some[string](source.substr(start, here))
   start = here

func read_currency(source: string; start: var int): Option[Token] =
   let valid = 0..source.high
   if start notin valid: return

   var here = start
   let currency = read_currency_name(source, here)
   let amount = read_int(source, here)
   if amount.is_some:
      result = some[Token](Token(kind: tkMoney, name: currency, amount: amount.get))
      start = here

func read_url(source: string; start: var int): Option[string] =
   let valid = 0..source.high
   if start notin valid: return
   var here = start
   # scheme://host:port/path?query-string#fragment-id
   # https://www.tutorialrepublic.com/html-tutorial/html-url.php
   let scheme = read_ident(source, here)
   if scheme.is_none: return
   # check for the //
   if here+1 notin valid: return
   if source[here] != '/' and source[here+1] != '/': return
   inc here, 2
   # we're just going to punt and consume until whitespace
   while here in valid:
      if is_whitespace(source[here]): break
      inc here
   if here > start:
      var value = if here in valid:
            source.substr(start, here-1)
         else:
            source.substr(start, here)
      result = some[string](value)
      start = here

# TODO allow long strings inside nested {}'s
# TODO delimited strings like {foo{...}foo}
# XXX apparently Red has %%{..}%% where number of percents must match

iterator lexer*(source: string; here: var int): Token =
   var output: Token
   var last = tkError
   while here in 0..source.high:
      block think:
         block simple_symbol:
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
               break simple_symbol
            inc here
            break think

         let url = read_url(source, here)
         if url.is_some:
            output = Token(kind: tkUrl, sdata: url.get)
            break think

         case last
         of tkOpenBrace:
            let blob = read_binary(source, here)
            if blob.is_some:
               output = Token(kind: tkBinary, sdata: blob.get)
               break think
         of tkInteger:
            if source[here] == 'x':
               output = Token(kind: tkX)
               inc here
               break think
            elif source[here] == '-':
               output = Token(kind: tkIdentifier, sdata: "-")
               inc here
               break think
         of tkHash:
            let anystr = read_anystring(source, here)
            if anystr.is_some:
               output = Token(kind: tkIdentifier, sdata: anystr.get)
               break think
         of tkPercent:
            let anystr = read_anystring(source, here)
            if anystr.is_some:
               output = Token(kind: tkIdentifier, sdata: anystr.get)
               break think
         else: discard

         let whitespace = read_whitespace(source, here)
         if whitespace.is_some:
            output = Token(kind: tkWhitespace, lines: whitespace.get)
            break think

         let comment = read_comment(source, here)
         if comment.is_some:
            output = Token(kind: tkComment, sdata: comment.get)
            break think

         let integer = read_int(source, here)
         if integer.is_some:
            output = Token(kind: tkInteger, idata: integer.get)
            break think

         # strings are a bit bizzare to deal with
         case last
         of tkQuote, tkStringFragment, tkStringEscape:
            if source[here] == '"':
               output = Token(kind: tkQuote)
               inc here
               break think
            elif source[here] == '^':
               let escaped = read_strescape(source, here)
               if escaped.is_none:
                  output = Token(kind: tkError)
               else:
                  output = Token(kind: tkStringEscape, sdata: escaped.get)
               break think
            else:
               let frag = read_strfrag(source, here)
               if frag.is_none:
                  output = Token(kind: tkError)
               else:
                  output = Token(kind: tkStringFragment, sdata: frag.get)
               break think
         else:
            if source[here] == '"':
               output = Token(kind: tkQuote)
               inc here
               break think

         # check for a plain old identifier
         var ident = read_ident(source, here)
         if ident.is_some:
            output = Token(kind: tkIdentifier, sdata: ident.get)
            break think

      last = output.kind
      yield output
      if last == tkError: break

when is_main_module:
   var code = "vig: $BTC500 bub: $50.0 site: sptp://shitpost.blob.cat user: @icedquinn@blob.cat map: #(bing: bong) file: %pleroma.vrf seymore \"oh ye gods ^(steamed ham)\" jingle: 'jangle author: @icedquinn@blob.cat ; henlo fediblobs\n out: sample2d texture uv/xy soup [44 22] jingle: 92 + 7 450x750"
   var marker = 0
   for token in lexer(code, marker):
      echo token

