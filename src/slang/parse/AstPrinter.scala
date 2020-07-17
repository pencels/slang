package slang.parse

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
  def print(indent: Int, expr: Expr, withSemi: Boolean = false): String = {
    expr match {
      case Expr.Assign(left, right) => print(left) + " = " + print(indent, right)
      case Expr.Binary(left, op, right) => print(left) + " " + op.lexeme + " " + print(right)
      case Expr.Block(expr) => printMaybeIndented(indent, print(indent + 1, expr), "{", "}", spaced = true)
      case Expr.Matchbox(matches) => printMaybeIndented(indent, matches.map(print(indent, _)), "{", "", "}", spaced = true)
      case Expr.Call(left, args) => (left :: args).map(print(indent, _)).mkString(" ")
      case Expr.Grouping(inner) => printMaybeIndented(indent, print(indent + 1, inner), "(", ")", spaced = false)
      case Expr.Id(name) => name
      case Expr.Literal(value) => value.toString
      case Expr.Postfix(expr, op) => print(expr) + op.lexeme
      case Expr.List(elements) => elements.map(print).mkString("[", ", ", "]")
      case Expr.Prefix(op, expr) => op.lexeme + print(expr)
      case Expr.Let(pattern, init) => "let " + print(pattern) + " = " + print(indent, init)
      case Expr.Print(expr) => "print " + print(indent, expr)
      case Expr.MatchRow(patterns, guard, expr) => printRow(indent, patterns, guard, expr)
      case Expr.Seq(exprs) => printSeq(indent, exprs.map(print(indent, _)), withSemi)
    }
  }

  def print(pattern: Pattern): String = {
    pattern match {
      case Pattern.Id(name) => name.lexeme
      case Pattern.Ignore(token) => "_"
      case Pattern.Strict(inner, full) => (if (full) "!" else "") + "{ " + print(inner) + " }"
      case Pattern.Literal(value) => value.toString
      case Pattern.List(patterns) => patterns.map(print).mkString("[", ", ", "]")
      case Pattern.Cons(head, tail) => s"(${print(head)} . ${print(tail)})"
      case Pattern.Spread(name) => name.lexeme + ".."
    }
  }

  def print(value: Value): String = print(0, value)

  def print(indent: Int, value: Value): String = {
    value match {
      case Value.Lazy(environment, expr) => {
        environment.shortString + " " + printMaybeIndented(indent, print(indent + 1, expr), "{", "}", spaced = true)
      }
      case Value.Matchbox(rows) => {
        val rowStrs = rows.map(print(indent + 1, _))
        printMaybeIndented(indent, rowStrs, "{", "", "}", spaced = true)
      }
      case Value.Hashbox(partialArguments, innerEnvironment, arity, rows, extraRow) => {
        val rowsStrs = rows.map(print(indent + 1, _))
        val allRows = rowsStrs ++ extraRow.map(r => List(print(indent + 1, r))).getOrElse(List())
        val partialArgsStr = if (partialArguments.length == 0) "" else partialArguments.mkString("[", ", ", "]") + " @ "
        partialArgsStr + innerEnvironment.shortString + " #" + printMaybeIndented(indent, allRows.toList, "{", "", "}", spaced = true)
      }
      case Value.List(values) => values.map(print).mkString("[", ", ", "]")
      case _ => value.toString
    }
  }

  def printRow(indent: Int, patterns: List[Pattern], guard: Option[Expr], expr: Expr): String = {
    val guardStr = guard map { " | " + print(_) } getOrElse ""
    patterns.map(print).mkString(" ") + guardStr + " -> " + printRowResult(indent + 1, expr)
  }

  def print(indent: Int, row: (List[Value], Expr)): String = {
    val (params, expr) = row
    params.map(print).mkString(" ") + " -> " + printRowResult(indent + 1, expr)
  }

  def print(indent: Int, row: Value.Matchbox.Row): String = {
    val Value.Matchbox.Row(innerEnvironment, params, guard, result) = row
    innerEnvironment.shortString + " " + printRow(indent, params, guard, result)
  }

  def print(indent: Int, row: Value.Hashbox.Row): String = {
    val Value.Hashbox.Row(params, result) = row
    printRow(indent, params, None, result)
  }

  def printRowResult(indent: Int, result: Expr): String = {
    result match {
      case Expr.Seq(exprs) => print(indent + 1, result, withSemi = true)
      case _ => print(indent, result)
    }
  }

  private def printMaybeIndented(indent: Int, innerStr: String, start: String, end: String, spaced: Boolean): String = {
    val spacing = if (spaced) " " else ""
    if (innerStr.contains('\n')) {
      start + "\n" + INDENT_STR * (indent + 1) + innerStr + "\n" + INDENT_STR * indent + end
    } else {
      start + spacing + innerStr + spacing + end
    }
  }

  private def printMaybeIndented(indent: Int, innerStrs: List[String], start: String, delim: String, end: String, spaced: Boolean): String = {
    if (innerStrs.length == 1) {
      printMaybeIndented(indent, innerStrs.head, start, end, spaced)
    } else {
      innerStrs
        .map(INDENT_STR * (indent + 1) + _)
        .mkString(start + "\n", delim + "\n", delim + "\n" + INDENT_STR * indent + end)
    }
  }

  private def printSeq(indent: Int, innerStrs: List[String], withSemi: Boolean = false): String = {
    val delim = if (withSemi) ";\n" else "\n"
    innerStrs match {
      case Nil => ""
      case List(head) => head
      case head :: tail => 
        head + delim + tail.map(INDENT_STR * indent + _).mkString("", delim, "")
    }
  }
}