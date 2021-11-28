import options, lexer, decodebin

type
   NodeKind* = enum
      nkError
      nkTypename
      nkGetWord
      nkSetWord
      nkSetPath
      nkGetPath
      nkWord
      nkBlock
      nkParenBlock
      nkString
      nkInteger
      nkFloat
      nkPair
      nkTuple
      nkEmail
      nkIssue
      nkReference
      nkPath
      nkRefinement
      nkPercent
      nkMapBlock
      nkFile
      nkBinary
      nkDate
      nkMoney
      nkUrl
      nkTag
      nkTime

   NodeFlag* = enum
      nfQuoted # Words and Paths can be quoted with '
      nfSelfClosedTag # Tag ends with />
      nfClosingTag # Tag's name begins with /

   Node* = ref object
      children: seq[Node]
      flags: set[NodeFlag]
      case kind: NodeKind
      of nkFile, nkWord, nkSetWord, nkGetWord, nkIssue, nkReference, nkTypename, nkString, nkBinary, nkUrl:
         sdata: string
      of nkInteger:
         idata: int
      of nkPercent, nkFloat:
         fdata: float
      of nkMoney:
         currency: Option[string]
      else: discard

   ParserState = enum
      psIdle
      psWord
      psWordInsidePath
      psEmail
      psHash
      psIssue
      psAt
      psReference
      psPath
      psGetPath
      psPathAwaitingWord
      psSetPath
      psSingleQuote
      psTypename
      psInteger
      psTuple
      psTupleNeedsFloat
      psBlock
      psBlockParen
      psPairNeedsX
      psPairNeedsInteger
      psPercent
      psFloat
      psQuotedStringInProgress
      psQuotedString
      psColon
      psGetWord
      psSetWord
      psMap
      psFileNeedsPayload
      psFile
      psBinaryNeedsPayload
      psBinaryNeedsOpen
      psBinaryNeedsClose
      psBinary
      psDateSlash
      psDateSlashNeedsPayload
      psDateDash
      psDateDashNeedsPayload
      psMoney
      psUrl
      psTag
      psTime
      psTimeNeedsSecond
      psTimeNeedsFraction

   Parser* = object
      state_stack: seq[ParserState]
      value_stack: seq[Node]

func reset*(self: var Parser) =
   var n: Node
   n = Node(kind: nkBlock)

   self.state_stack = @[psIdle]
   self.value_stack = @[n]

func vtop(self: Parser): Node =
   self.value_stack[self.value_stack.high]

func `vtop=`(self: var Parser; neu: Node) =
   self.value_stack[self.value_stack.high] = neu

func vpop(self: var Parser): Node =
   return self.value_stack.pop

func vpush(self: var Parser; node: Node) =
   debugecho "PUSH ", node.kind
   self.value_stack.add node

func top(self: Parser): ParserState =
   self.state_stack[self.state_stack.high]

func `top=`(self: var Parser; neu: ParserState) =
   debugecho "STATE ", self.top, "->", neu
   self.state_stack[self.state_stack.high] = neu

proc feed*(self: var Parser; token: Token) =
   func smash_float(aa, bb: int): float =
      var a = aa.float
      var b = bb.float

      # you shouldn't have negative mantissas anyway
      if b < 0: b *= -1
      while b > 0.9999:
         b *= 0.1

      return a + b

   template eject() =
      case self.top
      of psIdle, psHash, psAt, psSingleQuote, psPairNeedsInteger,
         psPathAwaitingWord, psQuotedStringInProgress,
         psBinaryNeedsPayload, psDateDashNeedsPayload, psDateSlashNeedsPayload,
         psColon, psTupleNeedsFloat, psFileNeedsPayload,
         psBinaryNeedsOpen, psBinaryNeedsClose, psTimeNeedsFraction,
         psTimeNeedsSecond:
            # TODO proper exception
            raise new_exception(Exception,
               "Parser jammed.")
      of psWordInsidePath:
         discard self.state_stack.pop
         echo "COMMIT ", self.vtop.kind
         var x = self.vpop
         self.vtop.children.add x
      of psTuple:
         self.top = psIdle
         if self.vtop.children.len == 2:
            let f = smash_float(
               self.vtop.children[0].idata,
               self.vtop.children[1].idata)
            self.vtop = Node(kind: nkFloat, fdata: f)
         else:
            echo "COMMIT ", self.vtop.kind
            var x = self.vpop
            self.vtop.children.add x

      of psWord, psIssue, psEmail, psSetPath, psQuotedString, psGetWord,
         psReference, psPath, psTypename, psInteger, psBlock, psPairNeedsX,
         psSetWord, psGetPath, psBlockParen, psPercent, psFloat, psMap,
         psFile, psBinary, psDateDash, psDateSlash, psMoney, psUrl, psTag,
         psTime:
            echo "COMMIT ", self.vtop.kind
            self.top = psIdle
            var x = self.vpop
            self.vtop.children.add x

   var slapfish = 0
   while true:
      # runaway loop detection at debug time
      inc slapfish
      assert slapfish < 100

      case token.kind
      of tkError:
         # TODO proper exception
         raise new_exception(Exception,
            "Parser jammed.")
      of tkEOF:
         eject()
         return
      of tkMoney:
         case self.top
         of psIdle:
            self.top = psMoney
            var money = Node(kind: nkMoney, currency: token.name)
            money.children.add Node(kind: nkInteger, idata: token.amount)
            self.value_stack.add money
            return
         else: eject() # TODO
      of tkUrl:
         case self.top
         of psIdle:
            self.top = psUrl
            self.value_stack.add Node(kind: nkUrl, sdata: token.sdata)
            return
         else: eject() # TODO
      of tkComment:
         return # comments don't matter here
      of tkSingleQuote:
         case self.top
         of psIdle:
            self.top = psSingleQuote
            return
         else: eject() # TODO
      of tkWhitespace:
         if self.top == psIdle: return
         eject()
      of tkStringFragment:
         case self.top
         of psQuotedStringInProgress:
            self.vtop.sdata.add token.sdata
            return
         else: eject()
      of tkStringEscape:
         case self.top:
         of psQuotedStringInProgress:
            echo "TODO string escape sequence not processed"
            return
         else: eject()
      of tkBinary:
         case self.top:
         of psBinaryNeedsPayload:
            self.top = psBinaryNeedsClose
            let base = self.vpop
            case base.idata
            of 2:
               self.value_stack.add(
                  Node(
                     kind: nkBinary,
                     sdata: decode2b(token.sdata)))
            of 16:
               self.value_stack.add(
                  Node(
                     kind: nkBinary,
                     sdata: decode16b(token.sdata)))
            of 64:
               self.value_stack.add(
                  Node(
                     kind: nkBinary,
                     sdata: decode64b(token.sdata)))
            else:
               # TODO better exception
               raise new_exception(Exception, "Can only read binary strings of radix 2, 16, or 64.")
            return
         else: eject() # TODO
      of tkInteger:
         case self.top
         of psTimeNeedsFraction:
            self.top = psTime
            if self.vtop.children.len < 1:
               raise new_exception(Exception, "Jammed: fraction needs an integer.")

            let f = smash_float(
               self.vtop.children[self.vtop.children.high].idata,
               token.idata)
            var n = Node(kind: nkFloat, fdata: f)
            discard self.vtop.children.pop
            self.vtop.children.add n
            return
         of psTimeNeedsSecond:
            self.top = psTime
            self.vtop.children.add Node(kind: nkInteger, idata: token.idata)
            return 
         of psDateSlashNeedsPayload:
            self.top = psDateSlash
            self.vtop.children.add Node(kind: nkInteger, idata: token.idata)
            return
         of psDateDashNeedsPayload:
            self.top = psDateDash
            self.vtop.children.add Node(kind: nkInteger, idata: token.idata)
            return
         of psIdle:
            self.top = psInteger
            var node = Node(kind: nkInteger, idata: token.idata)
            self.value_stack.add node
            return
         of psTupleNeedsFloat:
            self.top = psTuple
            self.vtop.children.add Node(kind: nkInteger, idata: token.idata)
            return
         of psPairNeedsInteger:
            self.top = psPairNeedsX
            var node = Node(kind: nkInteger, idata: token.idata)
            self.vtop.children.add node
            return
         else: eject()
      of tkPercent:
         case self.top
         of psIdle:
            self.top = psFileNeedsPayload
            return
         of psInteger:
            self.top = psPercent
            self.vtop = Node(kind: nkPercent, fdata: self.vtop.idata.float * 0.01)
            return
         of psTuple:
            self.top = psPercent
            let f = smash_float(
               self.vtop.children[0].idata,
               self.vtop.children[1].idata)
            self.vtop = Node(kind: nkPercent, fdata: f * 0.01)
            return
         else: eject()
      of tkHash:
         case self.top
         of psInteger:
            self.top = psBinaryNeedsOpen
            return
         of psIdle:
            self.top = psHash
            return
         else: eject()
      of tkDollar: eject() # TODO
      of tkOpenBrace:
         case self.top
         of psBinaryNeedsOpen:
            self.top = psBinaryNeedsPayload
            return
         else: eject()
      of tkCloseBrace:
         case self.top
         of psBinaryNeedsClose:
            self.top = psBinary
            return
         else: eject()
      of tkOpenParenthesis:
         case self.top
         of psHash:
            # TODO check overflow policy
            echo "PUSH DEPTH"
            self.top = psMap
            self.state_stack.add psIdle
            var node = Node(kind: nkMapBlock)
            self.value_stack.add node
            return
         of psIdle:
            # TODO check overflow policy
            echo "PUSH DEPTH"
            self.top = psBlockParen
            self.state_stack.add psIdle
            var node = Node(kind: nkParenBlock)
            self.value_stack.add node
            return
         else: eject()
      of tkCloseParenthesis:
         case self.top
         of psIdle:
            echo "POP DEPTH"
            if self.value_stack.len > 1:
               case self.state_stack[self.state_stack.high-1]
               of psBlockParen, psMap:
                  discard self.state_stack.pop
               else:
                  raise new_exception(Exception, "Closing block mismatch.")
               return
            else:
               # TODO
               raise new_exception(Exception, "Underflowed.")
         else: eject()
      of tkOpenBracket:
         case self.top
         of psIdle:
            # TODO check overflow policy
            echo "PUSH DEPTH"
            self.top = psBlock
            self.state_stack.add psIdle
            var node = Node(kind: nkBlock)
            self.value_stack.add node
            return
         else: eject()
      of tkCloseBracket:
         case self.top
         of psIdle:
            echo "POP DEPTH"
            if self.value_stack.len > 1:
               if self.state_stack[self.state_stack.high-1] != psBlock:
                  raise new_exception(Exception, "Closing block mismatch.")
               else:
                  discard self.state_stack.pop
               return
            else:
               # TODO
               raise new_exception(Exception, "Underflowed.")
         else: eject()
      of tkOpenAngle:
         case self.top
         of psIdle:
            # TODO check overflow policy
            echo "PUSH DEPTH"
            self.top = psTag
            self.state_stack.add psIdle
            var node = Node(kind: nkTag)
            self.value_stack.add node
            return
         else: eject()
      of tkCloseAngle:
         echo "SHIT ", self.top
         case self.top
         of psIdle, psPathAwaitingWord:
            echo "POP DEPTH"
            if self.value_stack.len > 1:
               echo "FART ", self.vtop[]
               if self.top == psPathAwaitingWord:
                  discard self.state_stack.pop
                  # we need to potentially fix a word being turned in to
                  # a path incorrectly by an earlier parse step
                  if self.vtop.children.len == 0:
                     discard self.vpop
                     incl self.vtop.flags, nfSelfClosedTag
                  elif self.vtop.children.len == 1:
                     # is a word which was improperly boxed
                     var bruh = self.vtop.children[0]
                     discard self.vpop
                     if self.state_stack[self.state_stack.high-1] != psTag:
                        raise new_exception(Exception, "Closing block mismatch.")
                     echo "FART ", self.vtop[]
                     incl self.vtop.flags, nfSelfClosedTag
                     self.vtop.children.add bruh
               discard self.state_stack.pop
               return
            else:
               # TODO
               raise new_exception(Exception, "Underflowed.")
         else: eject()
      of tkIdentifier:
         case self.top
         of psInteger:
            if token.sdata.len != 1 or token.sdata[0] != '-':
               eject()
               continue
            self.top = psDateDashNeedsPayload
            var node = Node(kind: nkDate)
            node.children.add self.vpop
            self.value_stack.add node
            return
         of psDateDash:
            if token.sdata.len != 1 or token.sdata[0] != '-':
               eject()
               continue
            self.top = psDateDashNeedsPayload
            return
         of psFileNeedsPayload:
            self.top = psFile
            var word = Node(kind: nkFile, sdata: token.sdata)
            self.vpush word
            return
         of psIdle:
            var word: Node
            if token.sdata[token.sdata.high] == ':':
               self.top = psSetWord
               word = Node(kind: nkSetWord, sdata: token.sdata.substr(0, token.sdata.high-1))
            else:
               self.top = psWord
               word = Node(kind: nkWord, sdata: token.sdata)
            self.vpush word
            return
         of psPathAwaitingWord:
            self.top = psWordInsidePath
            var word: Node
            word = Node(kind: nkWord, sdata: token.sdata)
            self.vpush word
            return
         of psWord, psWordInsidePath:
            self.vtop.sdata.add token.sdata
            return
         of psSingleQuote:
            self.top = psWord
            var word = Node(kind: nkWord, flags: {nfQuoted}, sdata: token.sdata)
            self.vpush word
            return
         of psHash:
            self.top = psIssue
            var issue: Node
            issue = Node(kind: nkIssue, sdata: token.sdata)
            self.vpush issue
            return
         of psAt:
            self.top = psReference
            var reference: Node
            reference = Node(kind: nkReference, sdata: token.sdata)
            self.vpush reference
            return
         of psColon:
            self.top = psGetWord
            var getword: Node
            getword = Node(kind: nkGetWord, sdata: token.sdata)
            self.vpush getword
            return
         else: eject()
      of tkPathSeparator:
         case self.top
         of psInteger:
            self.top = psDateSlashNeedsPayload
            var node = Node(kind: nkDate)
            node.children.add self.vpop
            self.value_stack.add node
            return
         of psDateSlash:
            self.top = psDateSlashNeedsPayload
            return
         of psIdle:
            # possibly this means we have started a closing tag
            if self.value_stack.len > 1:
               case self.state_stack[self.state_stack.high-1]
               of psTag:
                  if self.vtop.children.len == 0:
                     incl self.vtop.flags, nfClosingTag
                     return
               else: discard
            # if we haven't bailed by now, we aren't starting a closing tag
            self.top = psPath
            var path = Node(kind: nkRefinement)
            self.value_stack.add path
            self.state_stack.add psPathAwaitingWord
            return
         of psGetWord:
            echo "REWRITE get-path"
            self.top = psGetPath
            var path = Node(kind: nkGetPath)
            path.children.add self.vpop
            path.children[0].kind = nkWord
            self.value_stack.add path
            self.state_stack.add psPathAwaitingWord
            return
         of psWord:
            echo "REWRITE path"
            self.top = psPath
            var path = Node(kind: nkPath)
            path.children.add self.vpop
            self.value_stack.add path
            self.state_stack.add psPathAwaitingWord
            return
         else: eject()
      of tkBang:
         case self.top:
         of psWord:
            echo "REWRITE type-name"
            self.top = psTypename
            self.vtop.kind = nkTypename
            return
         else: eject()
      of tkColon:
         case self.top:
         of psPath:
            echo "REWRITE set-path"
            self.top = psSetPath
            self.vtop.kind = nkSetPath
            return
         of psTime:
            self.top = psTimeNeedsSecond
            return
         of psInteger:
            self.top = psTimeNeedsSecond
            var n = Node(kind: nkTime, children: @[self.vpop])
            self.value_stack.add n
            return
         of psWord:
            echo "REWRITE set-word"
            self.vtop.kind = nkSetWord
            self.top = psSetWord
            return
         of psIdle:
            self.top = psColon
            return
         else: eject() # TODO
      of tkPeriod:
         case self.top:
         of psTime:
            self.top = psTimeNeedsFraction
            return
         of psInteger:
            self.top = psTupleNeedsFloat
            var node = Node(kind: nkTuple)
            node.children.add self.vpop
            self.value_stack.add node
            return
         of psTuple:
            self.top = psTupleNeedsFloat
            return
         else: eject()
      of tkAt:
         case self.top
         of psIdle:
            self.top = psAt
            return
         else: eject() # TODO
      of tkX:
         case self.top
         of psInteger:
            echo "REWRITE pair"
            self.top = psPairNeedsInteger
            var word = Node(kind: nkPair)
            word.children.add self.vpop
            self.value_stack.add word
            return
         of psPairNeedsX:
            self.top = psPairNeedsInteger
            return
         else: eject()
      of tkQuote:
         case self.top
         of psQuotedStringInProgress:
            self.top = psQuotedString
            return
         of psIdle:
            self.top = psQuotedStringInProgress
            var word = Node(kind: nkString)
            self.value_stack.add word
            return
         else: eject()

proc dump(self: Node) =
   if self == nil:
      echo "<nil>"
      return

   echo self[]
   echo "children >>"
   for x in self.children:
      dump(x)
   echo "<<"

proc read*(code: string): Node =
   ## Lexes and parses and entire string of Rhombus and returns the resulting block.
   ## Will throw exceptions on parsing failures.
   var marker = 0
   var parser: Parser
   reset(parser)
   for token in lexer(code, marker):
      echo "FEED ", token
      feed(parser, token)
   feed(parser, Token(kind: tkEOF))
   if parser.value_stack.len> 0:
      return parser.value_stack[0]

var code = "80:66:55.99 <bruh> <bruh /> <bruh/> </bruh> uri: blub://fuck.com:81/butt.html 2020-01-01 2/2/2021 64#{deadbeef} model: %/model/pleroma.vrf shitmap: #(jingle: 'jangle) orange-juice: 75% pickle: 44.9% (big pan) :fiddly/sticks @reference #big-fucking-issue-555 16#{deadBEEF} subject: \"oh ye gods, ^(ham)\" :cupertino 'bollywood  /shimmy/dingdong email: icedquinn@iceworks.cc branch: #master pixel xapel/xooxpr  author: @icedquinn@blob.cat ; henlo fediblobs\n out: sample2d texture uv/xy soup [44 22] jingle: 92 + 7 450x650"
dump(read(code))

