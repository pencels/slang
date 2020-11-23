package slang.parse

import slang.lex.{SourceFile, OperatorTrie}
import slang.ux.ErrorReporter

case class ParseContext(
    file: SourceFile,
    reporter: ErrorReporter,
    operatorTrie: OperatorTrie
)
