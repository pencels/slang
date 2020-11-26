package slang.parse

import slang.lex.OperatorTrie
import slang.ux.ErrorReporter
import slang.sourcemap.SourceMap

case class ParseContext(
    sourceMap: SourceMap,
    startPos: Int,
    reporter: ErrorReporter,
    operatorTrie: OperatorTrie
)
