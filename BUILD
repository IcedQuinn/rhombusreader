
gentest(
   name='decodebin',
   test_cmd='./decodebin',
   cmd='nim c -o:decodebin src/rhombusreader_internal/decodebin.nim',
   outs=['decodebin'],
   no_test_output=True,
   srcs=['src/rhombusreader_internal/decodebin.nim'],
)

gentest(
   name='lexer',
   test_cmd='./lexer',
   cmd='nim c -o:lexer src/rhombusreader_internal/lexer.nim',
   outs=['lexer'],
   no_test_output=True,
   srcs=['src/rhombusreader_internal/lexer.nim'],
)

gentest(
   name='parser',
   test_cmd='./rhombusreader',
   cmd='nim c -o:rhombusreader src/rhombusreader.nim',
   outs=['rhombusreader'],
   no_test_output=True,
   srcs=['src/rhombusreader.nim', 'src/rhombusreader_internal/lexer.nim', 'src/rhombusreader_internal/decodebin.nim'],
)

