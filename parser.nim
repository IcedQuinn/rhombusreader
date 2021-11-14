import lexer

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

   NodeFlag* = enum
      nfQuoted

   Node* = ref object
      children: seq[Node]
      flags: set[NodeFlag]
      case kind: NodeKind
      of nkWord, nkSetWord, nkGetWord, nkIssue, nkReference, nkTypename, nkString:
         sdata: string
      of nkInteger:
         idata: int
      of nkPercent, nkFloat:
         fdata: float
      else: discard

   ParserState = enum
      psIdle
      psWord
      psWordInsidePath
      psEmailNeedsHostname
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
      psBlock
      psBlockParen
      psPairNeedsX
      psPairNeedsInteger
      psPercent
      psQuotedStringInProgress
      psQuotedString
      psColon
      psGetWord
      psSetWord

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
   template eject() =
      case self.top
      of psIdle, psHash, psAt, psSingleQuote, psPairNeedsInteger,
         psPathAwaitingWord, psEmailNeedsHostname, psQuotedStringInProgress,
         psColon:
            # TODO proper exception
            raise new_exception(Exception,
               "Parser jammed.")
      of psWordInsidePath:
         discard self.state_stack.pop
         echo "COMMIT ", self.vtop.kind
         var x = self.vpop
         self.vtop.children.add x
      of psWord, psIssue, psEmail, psSetPath, psQuotedString, psGetWord,
         psReference, psPath, psTypename, psInteger, psBlock, psPairNeedsX,
         psSetWord, psGetPath, psBlockParen, psPercent:
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
      of tkBinary: eject() # TODO
      of tkInteger:
         case self.top
         of psIdle:
            self.top = psInteger
            var node = Node(kind: nkInteger, idata: token.idata)
            self.value_stack.add node
            return
         of psPairNeedsInteger:
            self.top = psPairNeedsX
            var node = Node(kind: nkInteger, idata: token.idata)
            self.vtop.children.add node
            return
         else: eject()
      of tkPercent:
         case self.top
         of psInteger:
            self.top = psPercent
            self.vtop = Node(kind: nkPercent, fdata: self.vtop.idata.float * 0.01)
            return
         else: eject()
      of tkHash:
         case self.top
         of psIdle:
            self.top = psHash
            return
         else: eject()
      of tkDollar: eject() # TODO
      of tkOpenBrace: eject() # TODO
      of tkCloseBrace: eject() # TODO
      of tkOpenParenthesis:
         case self.top
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
               if self.state_stack[self.state_stack.high-1] != psBlockParen:
                  raise new_exception(Exception, "Closing block mismatch.")
               else:
                  discard self.state_stack.pop
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
      of tkOpenAngle: eject() # TODO
      of tkCloseAngle: eject() # TODO
      of tkIdentifier:
         case self.top
         of psIdle:
            self.top = psWord
            var word: Node
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
         of psEmailNeedsHostname:
            self.top = psEmail
            var frag = Node(kind: nkWord, sdata: token.sdata)
            self.vtop.children.add frag
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
         of psIdle:
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
         of psWord:
            echo "REWRITE set-word"
            self.vtop.kind = nkSetWord
            self.top = psSetWord
            return
         of psIdle:
            self.top = psColon
            return
         else: eject() # TODO
      of tkPeriod: eject() # TODO
      of tkAt:
         case self.top
         of psIdle:
            self.top = psAt
            return
         of psWord:
            echo "REWRITE email"
            var node = Node(kind: nkEmail, children: @[self.vpop])
            self.value_stack.add node
            self.top = psEmailNeedsHostname
            return
         else: eject() # TODO
      of tkX:
         case self.top
         of psWord, psWordInsidePath:
            self.vtop.sdata.add "x"
            return
         of psIdle:
            self.top = psWord
            var word: Node
            word = Node(kind: nkWord, sdata: "x")
            self.vpush word
            return
         of psPathAwaitingWord:
            self.top = psWordInsidePath
            var word: Node
            word = Node(kind: nkWord, sdata: "x")
            self.vpush word
            return
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

var code = "orange-juice: 75% pickle: 44.9% (big pan) :fiddly/sticks @reference #big-fucking-issue-555 16#{deadBEEF} subject: \"oh ye gods, ^(ham)\" :cupertino 'bollywood  /shimmy/dingdong email: icedquinn@iceworks.cc branch: #master pixel xapel/xooxpr  author: @icedquinn@blob.cat ; henlo fediblobs\n out: sample2d texture uv/xy soup [44 22] jingle: 92 + 7 450x650"
var parser: Parser
reset(parser)

var marker = 0
for token in lexer(code, marker):
   echo "FEED ", token
   feed(parser, token)
feed(parser, Token(kind: tkEOF))

dump(parser.value_stack[0])

