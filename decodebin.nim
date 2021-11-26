
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
   # count how many bits are in source
   # figure out how many bits modulo a byte
   # first byte will be cut off after remainder
   # remaining bytes are stuffed with all eight bits
   let valid = 0..source.high
   var accum, here, shifts, hits = 0
   while here in valid:
      while here in valid and is_whitespace(source[here]): inc here
      if here notin valid: break
      if source[here] notin '0'..'1':
         # TODO better exception
         raise new_exception(Exception, "Not valid bit character.")
      inc hits
      inc here

   here = 0
   # count how many bits we have modulo a full byte
   # if that is more than zero then invert the number to know how many
   # bits we have to pretend to have already shifted
   shifts = hits mod 8
   if shifts > 0: shifts = 8 - shifts

   while here in valid:
      while here in valid and is_whitespace(source[here]): inc here
      if here notin valid: break

      var needle = 0
      if source[here] == '1': needle = 1
      inc here

      accum = (accum shl 1) + needle
      inc shifts

      if shifts >= 8:
         result.add accum.char
         accum = 0
         shifts = 0
   # we already took care of the stragglers so this should always be true
   assert shifts == 0

when is_main_module:
   block test:
      var a = "00001111"
      let ah = decode2b(a)
      assert ah[0].int == 15

      var b = "11111111"
      let bh = decode2b(b)
      assert bh[0].int == 255

      var c = "111111111111"
      let ch = decode2b(c)
      assert ch[0].int == 15
      assert ch[1].int == 255

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
   block test:
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
