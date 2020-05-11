package slang.parse;

import java.util.List;
import slang.lex.*;
import slang.parse.*;
import slang.runtime.*;

public abstract class Expr {
  public interface Visitor<R> {
    R visitAssignExpr(Assign expr);
    R visitBinaryExpr(Binary expr);
    R visitBlockExpr(Block expr);
    R visitMatchBlockExpr(MatchBlock expr);
    R visitCallExpr(Call expr);
    R visitGroupingExpr(Grouping expr);
    R visitIdExpr(Id expr);
    R visitLiteralExpr(Literal expr);
    R visitPostfixExpr(Postfix expr);
    R visitSeqExpr(Seq expr);
    R visitUnaryExpr(Unary expr);
  }
  public static class Assign extends Expr {
    public Assign(Pattern left, Expr right) {
      this.left = left;
      this.right = right;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitAssignExpr(this);
    }

    public final Pattern left;
    public final Expr right;
  }
  public static class Binary extends Expr {
    public Binary(Expr left, Token op, Expr right) {
      this.left = left;
      this.op = op;
      this.right = right;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitBinaryExpr(this);
    }

    public final Expr left;
    public final Token op;
    public final Expr right;
  }
  public static class Block extends Expr {
    public Block(List<Stmt> statements) {
      this.statements = statements;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitBlockExpr(this);
    }

    public final List<Stmt> statements;
  }
  public static class MatchBlock extends Expr {
    public MatchBlock(List<Stmt.Match> matches) {
      this.matches = matches;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitMatchBlockExpr(this);
    }

    public final List<Stmt.Match> matches;
  }
  public static class Call extends Expr {
    public Call(Expr left, List<Expr> args) {
      this.left = left;
      this.args = args;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitCallExpr(this);
    }

    public final Expr left;
    public final List<Expr> args;
  }
  public static class Grouping extends Expr {
    public Grouping(Expr expr) {
      this.expr = expr;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitGroupingExpr(this);
    }

    public final Expr expr;
  }
  public static class Id extends Expr {
    public Id(String id) {
      this.id = id;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitIdExpr(this);
    }

    public final String id;
  }
  public static class Literal extends Expr {
    public Literal(Value value) {
      this.value = value;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitLiteralExpr(this);
    }

    public final Value value;
  }
  public static class Postfix extends Expr {
    public Postfix(Token op, Expr expr) {
      this.op = op;
      this.expr = expr;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitPostfixExpr(this);
    }

    public final Token op;
    public final Expr expr;
  }
  public static class Seq extends Expr {
    public Seq(List<Expr> elements) {
      this.elements = elements;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitSeqExpr(this);
    }

    public final List<Expr> elements;
  }
  public static class Unary extends Expr {
    public Unary(Token op, Expr expr) {
      this.op = op;
      this.expr = expr;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitUnaryExpr(this);
    }

    public final Token op;
    public final Expr expr;
  }

  public abstract <R> R accept(Visitor<R> visitor);
}
