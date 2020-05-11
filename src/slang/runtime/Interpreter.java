package slang.runtime;

import slang.lex.Token;
import slang.parse.Expr;
import slang.parse.Pattern;
import slang.parse.Stmt;
import slang.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;


public class Interpreter implements Expr.Visitor<Value>, Stmt.Visitor<Value> {
    private Environment environment = new Environment();

    public static final Value TRUE_ATOM = Value.Atom.create("true");
    public static final Value FALSE_ATOM = Value.Atom.create("false");

    public Value execute(Stmt stmt) {
        return stmt.accept(this);
    }

    public Value eval(Expr expr) {
        return expr.accept(this);
    }

    public Value eval(Expr expr, Environment withEnvironment) {
        Environment currentEnvironment = environment;
        environment = withEnvironment;
        Value value = eval(expr);
        environment = currentEnvironment;
        return value;
    }

    public Value evalTop(Expr expr) {
        Value value = eval(expr);
        if (value instanceof Lazy) {
            value = ((Lazy) value).getValue(this);
        }
        return value;
    }

    public Object evalTop(Expr expr, Environment withEnvironment) {
        Environment currentEnvironment = environment;
        environment = withEnvironment;
        Object value = evalTop(expr);
        environment = currentEnvironment;
        return value;
    }

    public Value interpret(List<Stmt> statements) {
        Value ret = Value.Nothing.getInstance();
        //try {
            for (Stmt stmt : statements) {
                ret = execute(stmt);
            }
        //} catch (RuntimeError error) {
        //    Slang.runtimeError(error);
        //}
        return ret;
    }

    public Value interpret(List<Stmt> statements, Environment withEnvironment) {
        Environment currentEnvironment = environment;
        environment = withEnvironment;
        Value value = interpret(statements);
        environment = currentEnvironment;
        return value;
    }

    @Override
    public Value visitAssignExpr(Expr.Assign expr) {
        Value value = expr.right.accept(this);
        if (!destructureAssignment(expr.left, value, environment)) {
            throw new RuntimeError(null, "Match failed.");
        }
        return value;
    }

    @Override
    public Value visitBinaryExpr(Expr.Binary expr) {
        Value left = evalTop(expr.left);
        Value right = evalTop(expr.right);

        switch (expr.op.type) {
            case AT:
                if (!(right instanceof Matchbox)) {
                    throw new RuntimeError(null, "Expected matchbox.");
                }
                return ((Matchbox) right).apply(this, List.of(left));
            case PLUS:
                if (left instanceof Value.SlangString) {
                    return new Value.SlangString(left.asString() + right.asString());
                } else if (left instanceof Value.SlangList && right instanceof Value.SlangList) {
                    // Concatenate lists.
                    List<Value> leftList = ((Value.SlangList) left).getValues();
                    List<Value> rightList = ((Value.SlangList) right).getValues();
                    List<Value> concat = new ArrayList<>(leftList);
                    concat.addAll(rightList);
                    return new Value.SlangList(concat);
                }
                checkNumberOperands(expr.op, left, right);
                return new Value.Double(left.asDouble() + right.asDouble());
            case MINUS:
                checkNumberOperands(expr.op, left, right);
                return new Value.Double(left.asDouble() - right.asDouble());
            case STAR:
                checkNumberOperands(expr.op, left, right);
                return new Value.Double(left.asDouble() * right.asDouble());
            case SLASH:
                checkNumberOperands(expr.op, left, right);
            return new Value.Double(left.asDouble() / right.asDouble());
            case EQEQ:
                if (left.equals(right)) {
                    return TRUE_ATOM;
                } else {
                    return FALSE_ATOM;
                }
            case NE:
                if (left.equals(right)) {
                    return FALSE_ATOM;
                } else {
                    return TRUE_ATOM;
                }
            case LT:
                checkNumberOperands(expr.op, left, right);
                if (left.asDouble() < right.asDouble()) {
                    return TRUE_ATOM;
                } else {
                    return FALSE_ATOM;
                }
            case LE:
                checkNumberOperands(expr.op, left, right);
                if (left.asDouble() <= right.asDouble()) {
                    return TRUE_ATOM;
                } else {
                    return FALSE_ATOM;
                }
            case GT:
                checkNumberOperands(expr.op, left, right);
                if (left.asDouble() > right.asDouble()) {
                    return TRUE_ATOM;
                } else {
                    return FALSE_ATOM;
                }
            case GE:
                checkNumberOperands(expr.op, left, right);
                if (left.asDouble() >= right.asDouble()) {
                    return TRUE_ATOM;
                } else {
                    return FALSE_ATOM;
                }
        }
        return null;
    }

    private void checkNumberOperand(Token op, Value operand) {
        if (operand instanceof Value.Double) return;
        throw new RuntimeError(op, "Expected operand to be number.");
    }

    private void checkNumberOperands(Token op, Value left, Value right) {
        if (left instanceof Value.Double && right instanceof Value.Double) return;
        throw new RuntimeError(op, "Expected operands to be numbers.");
    }

    private Value call(Value callee, List<Value> args) {
        if (callee instanceof Lazy) {
            callee = ((Lazy) callee).getValue(this);
        }

        if (callee instanceof Matchbox) {
            return ((Matchbox) callee).apply(this, args);
        } else if (callee instanceof MatchTree) {
            return ((MatchTree) callee).apply(this, args);
        } else if (callee instanceof Value.SlangList) {
            Value index = args.get(0);
            if (!(index instanceof Value.Double && Math.floor(index.asDouble()) == index.asDouble())) {
                throw new RuntimeError(null, "List index must be an integer.");
            }

            Value elem = ((Value.SlangList) callee).getValues().get((int) index.asDouble());
            List<Value> restArgs = args.subList(1, args.size());
            if (restArgs.isEmpty()) {
                return elem;
            } else {
                return call(elem, restArgs);
            }
        } else {
            throw new RuntimeError(null, "Left expr of call must be a Matchbox.");
        }
    }

    @Override
    public Value visitCallExpr(Expr.Call expr) {
        Value closure = eval(expr.left);
        List<Value> args = expr.args.stream().map(argExpr -> eval(argExpr)).collect(Collectors.toList());
        return call(closure, args);
    }

    @Override
    public Value visitGroupingExpr(Expr.Grouping expr) {
        return expr.expr.accept(this);
    }

    @Override
    public Value visitIdExpr(Expr.Id expr) {
        return environment.get(expr.id);
    }

    @Override
    public Value visitLiteralExpr(Expr.Literal expr) {
        return expr.value;
    }

    @Override
    public Value visitPostfixExpr(Expr.Postfix expr) {
        Value value = eval(expr.expr);
        switch (expr.op.type) {
            case BANG:
                checkNumberOperand(expr.op, value);
                int i = ((Double) value.asDouble()).intValue();
                double n = 1;
                for (; i > 0; i--) {
                    n *= i;
                }
                return new Value.Double(n);
        }
        return null;
    }

    @Override
    public Value visitSeqExpr(Expr.Seq expr) {
        return new Value.SlangList(expr.elements.stream().map(this::eval).collect(Collectors.toList()));
    }

    @Override
    public Value visitUnaryExpr(Expr.Unary expr) {
        // TODO: implement unary operators
        return null;
    }

    @Override
    public Value visitBlockExpr(Expr.Block block) {
        Environment currentEnvironment = environment; // TODO: is this the one we actually want...
        return new Lazy(block.statements, currentEnvironment);
    }

    @Override
    public Value visitMatchBlockExpr(Expr.MatchBlock expr) {
        //return new Matchbox(expr.matches, environment, true);
        return new MatchTree(expr.matches, environment);
    }

    @Override
    public Value visitExpressionStmt(Stmt.Expression stmt) {
        Value value = evalTop(stmt.expr);
        if (stmt.expr instanceof Expr.Assign)
            return Value.Nothing.getInstance();
        else
            return value;
    }

    @Override
    public Value visitLetStmt(Stmt.Let stmt) {
        Value init = eval(stmt.init);
        destructureAssignment(stmt.pattern, init, environment);
        return Value.Nothing.getInstance();
    }

    @Override
    public Value visitPrintStmt(Stmt.Print stmt) {
        System.out.println(evalTop(stmt.expr).asString());
        return Value.Nothing.getInstance();
    }

    @Override
    public Value visitMatchStmt(Stmt.Match stmt) {
        return null;
    }

    public Environment destructureAssignment(List<Pattern> patterns, List<Value> args, Environment parent) {
        // Create a new env to hold new bindings.
        Environment env = new Environment(parent);

        int n = Math.min(patterns.size(), args.size());
        for (int i = 0; i < n; i++) {
            if (!destructureAssignment(patterns.get(i), i, args, env))
                return null;
        }
        return env;
    }

    public boolean destructureAssignment(Pattern pattern, Value arg, Environment env) {
        if (pattern instanceof Pattern.Id) {
            env.define(((Pattern.Id) pattern).id.lexeme, arg);
            return true;
        }

        if (pattern instanceof Pattern.Ignore) {
            return true;
        }

        if (pattern instanceof Pattern.Lazy) {
            return destructureAssignment(((Pattern.Lazy) pattern).inner, arg, env);
        }

        if (pattern instanceof Pattern.Literal) {
            // Collapse lazy to their cached value.
            if (arg instanceof Lazy) {
                arg = ((Lazy) arg).getCachedValue(this);
            }

            Value patternValue = (Value) ((Pattern.Literal) pattern).literal.value;
            return patternValue.equals(arg);
        }

        if (pattern instanceof Pattern.Seq) {
            // Collapse lazy to their cached value.
            if (arg instanceof Lazy) {
                arg = ((Lazy) arg).getCachedValue(this);
            }

            List<Pattern> patterns = ((Pattern.Seq) pattern).patterns;
            // Must be matching a SlangList.
            if (arg instanceof Value.SlangList) {
                List<Value> values = ((Value.SlangList) arg).getValues();

                // Check empty list pattern.
                if (patterns.isEmpty()) return values.isEmpty();

                int n = patterns.size();
                Pattern lastPattern = patterns.get(n - 1);

                // Must have same length.
                // TODO: handle spread operator
                if (n == values.size() || (lastPattern instanceof Pattern.Spread && n - 1 <= values.size())) {
                    if (lastPattern instanceof Pattern.Spread) {
                        n--;
                    }
                    // Destructure for each pattern elem.
                    for (int i = 0; i < n; i++) {
                        if (!destructureAssignment(patterns.get(i), values.get(i), env))
                            return false;
                    }
                    if (lastPattern instanceof Pattern.Spread) {
                        return destructureAssignment(lastPattern, n, values, env);
                    }
                    return true;
                }
            }
            return false;
        }

        return false;
    }

    private boolean destructureAssignment(Pattern pattern, int i, List<Value> args, Environment env) {
        if (pattern instanceof Pattern.Spread) {
            env.define(((Pattern.Spread) pattern).id.lexeme, new Value.SlangList(args.subList(i, args.size())));
            return true;
        }

        return destructureAssignment(pattern, args.get(i), env);
    }
}
