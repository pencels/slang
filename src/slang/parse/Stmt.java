package slang.parse;

import java.util.List;
import slang.lex.*;
import slang.parse.*;
import slang.runtime.*;

public abstract class Stmt {
  public interface Visitor<R> {
    R visitExpressionStmt(Expression stmt);
    R visitLetStmt(Let stmt);
    R visitPrintStmt(Print stmt);
    R visitMatchStmt(Match stmt);
  }
  public static class Expression extends Stmt {
    public Expression(Expr expr) {
      this.expr = expr;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitExpressionStmt(this);
    }

    public final Expr expr;
  }
  public static class Let extends Stmt {
    public Let(Pattern pattern, Expr init) {
      this.pattern = pattern;
      this.init = init;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitLetStmt(this);
    }

    public final Pattern pattern;
    public final Expr init;
  }
  public static class Print extends Stmt {
    public Print(Expr expr) {
      this.expr = expr;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitPrintStmt(this);
    }

    public final Expr expr;
  }
  public static class Match extends Stmt {
    public Match(List<Pattern> patterns, Expr expr) {
      this.patterns = patterns;
      this.expr = expr;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitMatchStmt(this);
    }

    public final List<Pattern> patterns;
    public final Expr expr;
  }

  public abstract <R> R accept(Visitor<R> visitor);
}
