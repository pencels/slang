package slang.runtime

import slang.parse.AstPrinter
import slang.parse.Pattern

class FailedMatchException(pattern: Pattern, value: Value) extends RuntimeException("Could not match value: " + value + " to pattern: " + new AstPrinter().print(pattern))
