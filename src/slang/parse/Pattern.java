package slang.parse;

import java.util.List;
import slang.lex.*;
import slang.parse.*;
import slang.runtime.*;

public abstract class Pattern {
  public interface Visitor<R> {
    R visitIdPattern(Id pattern);
    R visitIgnorePattern(Ignore pattern);
    R visitLazyPattern(Lazy pattern);
    R visitLiteralPattern(Literal pattern);
    R visitSeqPattern(Seq pattern);
    R visitSpreadPattern(Spread pattern);
  }
  public static class Id extends Pattern {
    public Id(Token id) {
      this.id = id;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitIdPattern(this);
    }

    public final Token id;
  }
  public static class Ignore extends Pattern {
    public Ignore(Token ignore) {
      this.ignore = ignore;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitIgnorePattern(this);
    }

    public final Token ignore;
  }
  public static class Lazy extends Pattern {
    public Lazy(Pattern inner) {
      this.inner = inner;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitLazyPattern(this);
    }

    public final Pattern inner;
  }
  public static class Literal extends Pattern {
    public Literal(Token literal, Value value) {
      this.literal = literal;
      this.value = value;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitLiteralPattern(this);
    }

    public final Token literal;
    public final Value value;
  }
  public static class Seq extends Pattern {
    public Seq(List<Pattern> patterns) {
      this.patterns = patterns;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitSeqPattern(this);
    }

    public final List<Pattern> patterns;
  }
  public static class Spread extends Pattern {
    public Spread(Token id) {
      this.id = id;
    }

    public <R> R accept(Visitor<R> visitor) {
      return visitor.visitSpreadPattern(this);
    }

    public final Token id;
  }

  public abstract <R> R accept(Visitor<R> visitor);
}
