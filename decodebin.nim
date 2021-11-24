
const
   Hexadoodle = "0123456789ABCDEF"

static:
   assert char.sizeof == uint8.sizeof

func is_whitespace(ch: char): bool =
   # TODO share this with existing lexer code
   return ch == ' '

func is_hexchar(ch: char): bool =
   return ch in '0'..'9' or ch in 'A'..'F'

func hexchar_to_int(ch: char): int =
   if ch in '0'..'9':
      return ch.int - '0'.int
   if ch in 'A'..'F':
      return (ch.int - 'A'.int) + 10
   return -1

func normalize_hexchar(ch: char): char =
   ## Makes sure hexademical characters are upcased.
   if ch in 'a'..'f':
      return (ch.int - ('a'.int - 'A'.int)).char
   return ch

func decode2b(source: string): string =
   discard

func decode16b*(source: string): string =
   let valid = 0..source.high

   var here = 0
   var builder: uint8 = 0
   var hot = false

   # scan to see if we have odd number of hexes
   var hits = 0
   while here in valid:
      while here in valid and is_whitespace(source[here]): inc here # skip dumbness
      if here notin valid: break
      if is_hexchar(normalize_hexchar(source[here])): inc hits
      inc here

   # reset pointer; mark as hot to offset all things by one byte
   here = 0
   if hits mod 2 > 0: hot = true

   # now walk the string and decode
   while here in valid:
      # acquire pair
      while here in valid and is_whitespace(source[here]): inc here # skip dumbness
      if here notin valid: break

      let a = normalize_hexchar(source[here])
      # TODO find correct exception
      if not a.is_hexchar: raise new_exception(Exception, "Not hexadecimal character.")
      inc here

      if not hot:
         builder = hexchar_to_int(a).uint8
         hot = true
      else:
         builder = (builder shl 4) + hexchar_to_int(a).uint8
         result.add cast[char](builder)
         builder = 0
         hot = false

   if hot:
      builder = builder shl 4
      result.add cast[char](builder)

when is_main_module:
   var a = "fF"
   let ah = decode16b(a)
   assert ah[0].int == 255

   var b = "F"
   let bh = decode16b(b)
   assert bh[0].int == 15

   var c = "FF0F"
   let ch = decode16b(c)
   assert ch[0].int == 255
   assert ch[1].int == 15

func decode64b(source: string): string =
   discard
