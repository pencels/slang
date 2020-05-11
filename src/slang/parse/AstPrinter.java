package slang.parse;

import slang.runtime.Value;

public class AstPrinter implements Expr.Visitor<String>, Stmt.Visitor<String>, Pattern.Visitor<String> {
    public String print(Expr expr) {
        return expr.accept(this);
    }

    public String print(Stmt stmt) {
        return stmt.accept(this);
    }

    public String print(Pattern pattern) {
        return pattern.accept(this);
    }

    @Override
    public String visitAssignExpr(Expr.Assign expr) {
        return "(" + print(expr.left) + " = " + print(expr.right) + ")";
    }

    @Override
    public String visitBinaryExpr(Expr.Binary expr) {
        return parenthesize(expr.op.lexeme, expr.left, expr.right);
    }

    @Override
    public String visitCallExpr(Expr.Call expr) {
        StringBuilder builder = new StringBuilder();
        builder.append("(");
        builder.append(expr.left.accept(this));
        boolean first = true;
        for (Expr arg : expr.args) {
            builder.append(" ");
            builder.append(arg.accept(this));
        }
        builder.append(")");
        return builder.toString();
    }

    @Override
    public String visitGroupingExpr(Expr.Grouping expr) {
        return parenthesize("group", expr.expr);
    }

    @Override
    public String visitLiteralExpr(Expr.Literal expr) {
        return expr.value.toString();
    }

    @Override
    public String visitUnaryExpr(Expr.Unary expr) {
        return parenthesize(expr.op.lexeme, expr.expr);
    }

    @Override
    public String visitPostfixExpr(Expr.Postfix expr) {
        return parenthesize("post" + expr.op.lexeme, expr.expr);
    }

    @Override
    public String visitSeqExpr(Expr.Seq expr) {
        String str = "[";
        boolean first = true;
        for (Expr elem : expr.elements) {
            if (first) {
                first = false;
            } else {
                str += ", ";
            }
            str += print(elem);
        }
        str += "]";
        return str;
    }

    @Override
    public String visitIdExpr(Expr.Id expr) {
        return expr.id;
    }

    private String parenthesize(String name, Expr... exprs) {
        StringBuilder builder = new StringBuilder();

        builder.append("(").append(name);
        for (Expr expr : exprs) {
            builder.append(" ");
            builder.append(expr.accept(this));
        }
        builder.append(")");

        return builder.toString();
    }

    @Override
    public String visitBlockExpr(Expr.Block block) {
        String text = "{ ";
        var first = true;
        for (Stmt innerStmt : block.statements) {
            if (!first) {
                text += "\n";
            }
            text += print(innerStmt);
            if (first) {
                first = false;
            }
        }
        text += " }";
        return text;
    }

    @Override
    public String visitMatchBlockExpr(Expr.MatchBlock expr) {
        String text = "{";
        text += "\n";
        for (Stmt match : expr.matches) {
            text += "  " + print(match) + "\n";
        }
        text += "}";
        return text;
    }

    @Override
    public String visitExpressionStmt(Stmt.Expression stmt) {
        return stmt.expr.accept(this);
    }

    @Override
    public String visitLetStmt(Stmt.Let stmt) {
        return String.format("let %s = %s", print(stmt.pattern), stmt.init.accept(this));
    }

    @Override
    public String visitPrintStmt(Stmt.Print stmt) {
        return parenthesize("print", stmt.expr);
    }

    @Override
    public String visitMatchStmt(Stmt.Match stmt) {
        String patternStr = "";
        for (Pattern pattern : stmt.patterns) {
            patternStr += print(pattern) + " ";
        }
        return patternStr + "-> " + print(stmt.expr);
    }

    @Override
    public String visitIdPattern(Pattern.Id pattern) {
        return pattern.id.lexeme;
    }

    @Override
    public String visitIgnorePattern(Pattern.Ignore pattern) {
        return "_";
    }

    @Override
    public String visitLazyPattern(Pattern.Lazy pattern) {
        return "{ " + print(pattern.inner) + " }";
    }

    @Override
    public String visitLiteralPattern(Pattern.Literal pattern) {
        return pattern.literal.lexeme;
    }

    @Override
    public String visitSeqPattern(Pattern.Seq pattern) {
        String str = "[";
        boolean first = true;
        for (Pattern innerPat : pattern.patterns) {
            if (first) {
                first = false;
            } else {
                str += ", ";
            }
            str += print(innerPat);
        }
        str += "]";
        return str;
    }

    @Override
    public String visitSpreadPattern(Pattern.Spread pattern) {
        return pattern.id.lexeme + "..";
    }
}
