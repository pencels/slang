package slang.parse
import slang.parse.Expr._
import slang.parse.Pattern._
import slang.runtime._

class AstPrinter {
  private val INDENT_STR = "    ";

  def print(expr: Expr): String = print(0, expr)

  /**
    * Prints the expression with indentation.
    *
    * @param indent The number of indents that the expression is at.
    * @param expr The expression.
    * @return
    */
  def print(indent: Int, expr: Expr): String = {
    expr match {
      case Assign(left, right) => print(left) + " = " + print(indent, right)
      case Binary(left, op, right) => print(left) + " " + op.lexeme + " " + print(right)
      case Block(expr) => printMaybeIndented(indent, print(indent + 1, expr), "{", "\n", "}")
      case Expr.Matchbox(matches) => printMaybeIndented(indent, matches.map(print(indent + 1, _)), "{", "\n", "}")
      case Call(left, args) => (left :: args).map(print(indent, _)).mkString(" ")
      case Grouping(inner) => "(" + print(inner) + ")"
      case Expr.Id(name) => name
      case Expr.Literal(value) => value.toString
      case Postfix(expr, op) => print(expr) + op.lexeme
      case Expr.SlangList(elements) => elements.map(print).mkString("[", ", ", "]")
      case Prefix(op, expr) => op.lexeme + print(expr)
      case Let(pattern, init) => "let " + print(pattern) + " = " + print(indent, init)
      case Print(expr) => "print " + print(indent, expr)
      case MatchRow(patterns, expr) => patterns.map(print).mkString(" ") + " -> " + print(indent, expr)
      case Seq(exprs) => exprs.map(print(indent, _)).mkString(" ; ")
    }
  }

  def print(pattern: Pattern): String = {
    pattern match {
      case Pattern.Id(name) => name.lexeme
      case Ignore(token) => "_"
      case Strict(inner) => "{ " + print(inner) + " }"
      case Pattern.Literal(value) => value.toString
      case Pattern.SlangList(patterns) => patterns.map(print).mkString("[", ", ", "]")
      case Spread(name) => name.lexeme + ".."
    }
  }

  def print(value: Value): String = print(0, value)

  def print(indent: Int, value: Value): String = {
    value match {
      case Lazy(environment, expr) => {
        environment.shortString + " " + printMaybeIndented(indent, print(indent + 1, expr), "{", "\n", "}")
      }
      case slang.runtime.Matchbox(rows) => {
        val rowStrs = rows.map(print(indent + 1, _))
        printMaybeIndented(indent, rowStrs, "{", "\n", "}")
      }
      case Hashbox(partialArguments, innerEnvironment, arity, rows, extraRow) => {
        val rowsStrs = rows.map(print(indent, _))
        val allRows = rowsStrs ++ extraRow.map(r => List(print(indent + 1, r))).getOrElse(List())
        val partialArgsStr = if (partialArguments.length == 0) "" else partialArguments.mkString("[", ", ", "]") + " @ "
        partialArgsStr + innerEnvironment.shortString + "#" + printMaybeIndented(indent, allRows.toList, "{", "\n", "}")
      }
      case slang.runtime.SlangList(values) => values.map(print).mkString("[", ", ", "]")
      case _ => value.toString
    }
  }

  def print(indent: Int, row: (List[Value], Expr)): String = {
    val (params, expr) = row
    params.map(print).mkString(" ") + " -> " + print(indent + 1, expr)
  }

  def print(indent: Int, row: MatchboxRow): String = {
    val MatchboxRow(innerEnvironment, params, result) = row
    innerEnvironment.shortString + " " + params.map(print).mkString(" ") + " -> " + print(indent, result)
  }

  def print(indent: Int, row: HashboxRow): String = {
    val HashboxRow(params, result) = row
    params.map(print).mkString(" ") + " -> " + print(indent, result)
  }

  private def printMaybeIndented(indent: Int, innerStr: String, start: String, delim: String, end: String): String = {
    if (innerStr.contains('\n')) {
      start + delim + INDENT_STR * (indent + 1) + innerStr + delim + INDENT_STR * indent + end
    } else {
      start + " " + innerStr + " " + end
    }
  }

  private def printMaybeIndented(indent: Int, innerStrs: List[String], start: String, delim: String, end: String): String = {
    if (innerStrs.length == 1) {
      printMaybeIndented(indent, innerStrs.head, start, delim, end)
    } else {
      innerStrs
        .map(INDENT_STR * (indent + 1) + _)
        .mkString(start + delim, delim, delim + INDENT_STR * indent + end)
    }
  }
}