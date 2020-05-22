package slang.analysis;

import slang.parse.Expr;
import slang.parse.Stmt;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/*
public class UsedBindings implements Expr.Visitor<Set<String>>, Stmt.Visitor<Set<String>> {
    public Set<String> getBindings(Expr expr) {
        return expr.accept(this);
    }

    public Set<String> getBindings(Stmt stmt) {
        return stmt.accept(this);
    }

    public Set<String> getBindings(List<Stmt> stmts) {
        Set<String> bindings = new HashSet<>();
        for (Stmt stmt : stmts) {
            bindings.addAll(getBindings(stmt));
        }
        return bindings;
    }

    @Override
    public Set<String> visitAssignExpr(Expr.Assign expr) {
        return getBindings(expr.right);
    }

    @Override
    public Set<String> visitBinaryExpr(Expr.Binary expr) {
        Set<String> left = getBindings(expr.left);
        Set<String> right = getBindings(expr.right);
        left.addAll(right);
        return left;
    }

    @Override
    public Set<String> visitCallExpr(Expr.Call expr) {
        Set<String> left = getBindings(expr.left);
        for (Expr argExpr : expr.args) {
            Set<String> argBindings = getBindings(argExpr);
            left.addAll(argBindings);
        }
        return left;
    }

    @Override
    public Set<String> visitGroupingExpr(Expr.Grouping expr) {
        return getBindings(expr.expr);
    }

    @Override
    public Set<String> visitLiteralExpr(Expr.Literal expr) {
        return new HashSet<>();
    }

    @Override
    public Set<String> visitUnaryExpr(Expr.Unary expr) {
        return getBindings(expr.expr);
    }

    @Override
    public Set<String> visitPostfixExpr(Expr.Postfix expr) {
        return getBindings(expr.expr);
    }

    @Override
    public Set<String> visitSeqExpr(Expr.Seq expr) {
        Set<String> bindings = new HashSet<>();
        for (Expr elem : expr.elements) {
            bindings.addAll(getBindings(elem));
        }
        return bindings;
    }

    @Override
    public Set<String> visitIdExpr(Expr.Id expr) {
        Set<String> bindings = new HashSet<>();
        bindings.add(expr.id);
        return bindings;
    }

    @Override
    public Set<String> visitBlockExpr(Expr.Block block) {
        Set<String> bindings = new HashSet<>();
        for (Stmt stmt : block.statements) {
            bindings.addAll(getBindings(stmt));
        }
        return bindings;
    }

    @Override
    public Set<String> visitMatchBlockExpr(Expr.MatchBlock expr) {
        Set<String> bindings = new HashSet<>();
        for (Stmt stmt : expr.matches) {
            bindings.addAll(getBindings(stmt));
        }
        return bindings;
    }

    @Override
    public Set<String> visitExpressionStmt(Stmt.Expression stmt) {
        return getBindings(stmt.expr);
    }

    @Override
    public Set<String> visitLetStmt(Stmt.Let stmt) {
        return getBindings(stmt.init);
    }

    @Override
    public Set<String> visitPrintStmt(Stmt.Print stmt) {
        return getBindings(stmt.expr);
    }

    @Override
    public Set<String> visitMatchStmt(Stmt.Match stmt) {
        return getBindings(stmt.expr);
    }
}
*/