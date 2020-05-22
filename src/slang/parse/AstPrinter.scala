package slang.parse

case class AstPrinter() {
  def print(x: Any): String = x.toString

  /*
  def print(expr: Expr): String = {
    expr match {
      case Expr.Assign(left, right) => "(" + print(left) + " = " + print(right) + ")"
      case Expr.Binary(left, op, right) => parenthesize(op.lexeme, left, right)
      case Expr.Block(statements) => statements.map(print).mkString("{ ", "; ", " }")
      case Expr.Matchbox(matches) => ???
      case Expr.Call(left, args) => (print(left) :: args.map(print)).mkString("(", " ", ")")
      case Expr.Grouping(inner) => parenthesize("group", inner)
      case Expr.Id(name) => name
      case Expr.Literal(value) => value.toString
      case Expr.Postfix(expr, op) => parenthesize("post" + op.lexeme, expr)
      case Expr.SlangList(elements) => elements.map(print).mkString("[", ", ", "]")
      case Expr.Unary(op, expr) => parenthesize(op.lexeme, expr)
    }
  }

  def print(stmt: Stmt) = {
    stmt match {
      case Stmt.Expression(expr) =>
      case Stmt.Let(pattern, init) =>
      case Stmt.Print(expr) =>
      case Stmt.Match(patterns, expr) =>
    }
  }

  def print(pattern: Pattern) = {
    pattern match {
      case Pattern.Id(name) =>
      case Pattern.Ignore(token) =>
      case Pattern.Lazy(inner) =>
      case Pattern.Literal(token, value) =>
      case Pattern.SlangList(patterns) =>
      case Pattern.Spread(name) =>
    }
  }

  private def parenthesize(name: String, exprs: Expr*): String = (name :: exprs.toList.map(print)).mkString("(", " ", ")")

  def visitBlockExpr(block: Expr.Block) = {
    var text = "{ "
    var first = true
    import scala.collection.JavaConversions._
    for (innerStmt <- block.statements) {
      if (!first) text += "\n"
      text += print(innerStmt)
      if (first) first = false
    }
    text += " }"
    text
  }

  def visitMatchBlockExpr(expr: Nothing) = {
    var text = "{"
    text += "\n"
    import scala.collection.JavaConversions._
    for (`match` <- expr.matches) {
      text += "  " + print(`match`) + "\n"
    }
    text += "}"
    text
  }

  def visitExpressionStmt(stmt: Stmt.Expression) = stmt.expr.accept(this)

  def visitLetStmt(stmt: Stmt.Let) = String.format("let %s = %s", print(stmt.pattern), stmt.init.accept(this))

  def visitPrintStmt(stmt: Stmt.Print) = parenthesize("print", stmt.expr)

  def visitMatchStmt(stmt: Stmt.Match) = {
    var patternStr = ""
    import scala.collection.JavaConversions._
    for (pattern <- stmt.patterns) {
      patternStr += print(pattern) + " "
    }
    patternStr + "-> " + print(stmt.expr)
  }

  def visitIdPattern(pattern: Pattern.Id) = pattern.id.lexeme

  def visitIgnorePattern(pattern: Pattern.Ignore) = "_"

  def visitLazyPattern(pattern: Pattern.Lazy) = "{ " + print(pattern.inner) + " }"

  def visitLiteralPattern(pattern: Pattern.Literal) = pattern.literal.lexeme

  def visitSeqPattern(pattern: Nothing) = {
    var str = "["
    var first = true
    import scala.collection.JavaConversions._
    for (innerPat <- pattern.patterns) {
      if (first) first = false
      else str += ", "
      str += print(innerPat)
    }
    str += "]"
    str
  }

  def visitSpreadPattern(pattern: Pattern.Spread) = pattern.id.lexeme + ".."
  */
}