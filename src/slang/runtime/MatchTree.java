package slang.runtime;

import slang.parse.AstPrinter;
import slang.parse.Expr;
import slang.parse.Pattern;
import slang.parse.Stmt;
import slang.util.Pair;
import slang.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/*
public class MatchTree extends Value {
    private final Node root;
    private final Environment environment;

    public MatchTree(List<Stmt.Match> matches, Environment environment) {
        this.environment = environment;
        var node = new Node.Internal();
        for (var match : matches) {
            node.add(match.patterns, match.expr);
        }
        this.root = node;
    }

    private MatchTree(Node root, Environment environment) {
        this.root = root;
        this.environment = environment;
    }

    public Value apply(Interpreter interpreter, List<Value> args) {
        var resolution = root.resolve(interpreter, args, environment);
        if (resolution.isEmpty()) {
            throw new NoMatchException();
        }

        var stuff = resolution.get();
        if (stuff.node instanceof Node.Leaf) {
            var val = interpreter.eval(((Node.Leaf) stuff.node).expr, stuff.env);

            if (!stuff.remainingArgs.isEmpty()) {
                if (!(val instanceof MatchTree)) {
                    throw new RuntimeException("Callee must be MatchTree");
                }
                return ((MatchTree) val).apply(interpreter, stuff.remainingArgs);
            }

            return val;
        } else {
            return new MatchTree(stuff.node, stuff.env);
        }
    }

    @Override
    public String getType() {
        return "MatchTree";
    }

    @Override
    public String asString() {
        return toString();
    }

    @Override
    public String toString() {
        return root.toString();
    }

    // Holds the resolved node, env, and remaining args for a call to resolve()
    private static class Resolution {
        public final Node node;
        public final Environment env;
        public final List<Value> remainingArgs;

        public Resolution(Node node, Environment env, List<Value> remainingArgs) {
            this.node = node;
            this.env = env;
            this.remainingArgs = remainingArgs;
        }
    }

    private static class PatternMapping {
        public final Pattern pattern;
        public final Node node;

        private PatternMapping(Pattern pattern, Node node) {
            this.pattern = pattern;
            this.node = node;
        }

        @Override
        public String toString() {
            return node.toString();
        }
    }

    private static boolean isLiteral(Pattern pattern) {
        if (pattern instanceof Pattern.Literal) return true;

        if (pattern instanceof Pattern.Seq) {
            // Sequence is only literal if all of its sub-patterns are literal.
            return ((Pattern.Seq) pattern).patterns.stream().allMatch(MatchTree::isLiteral);
        }

        return false;
    }

    private static Value toLiteral(Pattern pattern) {
        if (pattern instanceof Pattern.Literal) return ((Pattern.Literal) pattern).value;

        if (pattern instanceof Pattern.Seq) {
            return new Value.SlangList(((Pattern.Seq) pattern).patterns.stream().map(MatchTree::toLiteral).collect(Collectors.toList()));
        }

        return null;
    }

    public static abstract class Node {
        public abstract int getMinHeight();
        public abstract void add(List<Pattern> patterns, Expr expr);
        public abstract Optional<MatchTree.Resolution> resolve(Interpreter interpreter, List<Value> args, Environment env);
        public abstract String prettyPrint(int depth);

        public static class WithEnv extends Node {
            private final Environment environment;
            private final Node node;

            public WithEnv(Environment environment, Node node) {
                this.environment = environment;
                this.node = node;
            }

            @Override
            public int getMinHeight() {
                return node.getMinHeight();
            }

            @Override
            public void add(List<Pattern> patterns, Expr expr) {
                node.add(patterns, expr);
            }

            @Override
            public Optional<Resolution> resolve(Interpreter interpreter, List<Value> args, Environment env) {
                // TODO: What are the semantics of using param env rather than the instance var env???
                return node.resolve(interpreter, args, env);
            }

            @Override
            public String prettyPrint(int depth) {
                var sb = new StringBuilder();
                sb.append(environment.shortString());
                sb.append(" ");
                sb.append(node.prettyPrint(depth));
                return sb.toString();
            }

            @Override
            public String toString() {
                return prettyPrint(0);
            }
        }

        public static class Internal extends Node {
            private Optional<Integer> minHeight = Optional.empty();
            public final Map<Value, Node> literals;
            public final Map<String, PatternMapping> patterns;

            public Internal() {
                literals = new HashMap<>();
                patterns = new HashMap<>();
            }

            public Internal(Map<Value, Node> literals, Map<String, PatternMapping> patterns) {
                this.literals = literals;
                this.patterns = patterns;
            }

            @Override
            public Optional<Resolution> resolve(Interpreter interpreter, List<Value> args, Environment env) {
                if (args.isEmpty()) {
                    return Optional.of(new Resolution(this, env, args));
                }

                var argsSize = args.size();
                var arg = args.get(0);
                var restArgs = args.subList(1, argsSize);

                // Deferring logic -- check for possible ambiguity.
                var matchesLiteral = literals.containsKey(arg);
                var matchingPatterns = patterns.entrySet().stream()
                        .map(pat -> {
                            // Use the node's env to start bindings instead of the parent env, if it's an env-holding node
                            var useEnv = env;
                            if (pat.getValue().node instanceof WithEnv) {
                                useEnv = ((WithEnv) pat.getValue().node).environment;
                            }
                            return new Pair<>(pat, new Environment(useEnv));
                        })
                        .filter(pair -> interpreter.destructureAssignment(pair.left.getValue().pattern, arg, pair.right))
                        .collect(Collectors.toList());
                var isAmbiguousFork = (matchesLiteral && matchingPatterns.size() > 0) || matchingPatterns.size() > 1;

                // If no ambiguity, go ahead with direct matching.
                Optional<Resolution> litResult = Optional.empty();
                if (matchesLiteral) {
                    litResult = literals.get(arg).resolve(interpreter, restArgs, env);
                }

                var subresults = matchingPatterns.stream()
                        .map(pair -> pair.left.getValue().node.resolve(interpreter, restArgs, pair.right))
                        .collect(Collectors.toList());

                var possibleMatches = Stream.concat(Stream.of(litResult), subresults.stream())
                        .filter(Optional::isPresent)
                        .map(Optional::get)
                        .collect(Collectors.toList());

                // No matches.
                if (possibleMatches.isEmpty()) {
                    return Optional.empty();
                }

                // Check if any leaves were hit. Greedily return first leaf hit.
                var leavesHit = possibleMatches.stream().filter(res -> res.node instanceof Node.Leaf);
                var firstLeaf = leavesHit.findFirst();
                if (firstLeaf.isPresent()) {
                    return firstLeaf;
                }

                // No leaves hit, that means we need to clone all the results with their envs into a new match tree.
                var newLiterals = possibleMatches.stream()
                        .flatMap(res -> ((Node.Internal) res.node).literals.entrySet().stream()
                                .map(entry -> new Pair<>(entry.getKey(), res.env.isEmpty() || res.env == env ? entry.getValue() : new Node.WithEnv(res.env, entry.getValue()))))
                        .collect(Collectors.toMap(pair -> pair.left, pair -> (Node) pair.right));
                var newPatterns = possibleMatches.stream()
                        .flatMap(res -> ((Node.Internal) res.node).patterns.entrySet().stream()
                                .map(entry -> new Pair<>(entry.getKey(), new PatternMapping(entry.getValue().pattern, res.env.isEmpty() || res.env == env ? entry.getValue().node : new Node.WithEnv(res.env, entry.getValue().node)))))
                        .collect(Collectors.toMap(pair -> pair.left, pair -> pair.right));

                // Return virtual new root with direct children having envs, remaining args 0 because we would have hit a leaf above if we had more args after a branch was resolved.
                var newRoot = new Internal(newLiterals, newPatterns);
                return Optional.of(new Resolution(newRoot, env, new ArrayList<>()));
            }

            @Override
            public String prettyPrint(int depth) {
                var indent = StringUtils.times(" ", depth + 2);
                var sb = new StringBuilder();
                sb.append("{ ");
                var first = true;
                for (var lit : literals.entrySet()) {
                    if (!first) {
                        sb.append("\n");
                        sb.append(indent);
                    }
                    sb.append(lit.getKey());
                    sb.append(" -> ");
                    sb.append(lit.getValue().prettyPrint(depth + lit.getKey().toString().length() + 6));
                    if (first) {
                        first = false;
                    }
                }
                for (var pat : patterns.entrySet()) {
                    if (!first) {
                        sb.append("\n");
                        sb.append(indent);
                    }
                    sb.append(pat.getKey());
                    sb.append(" -> ");
                    sb.append(pat.getValue().node.prettyPrint(depth + pat.getKey().length() + 6));
                    if (first) {
                        first = false;
                    }
                }
                sb.append(" }");
                return sb.toString();
            }

            @Override
            public int getMinHeight() {
                if (minHeight.isEmpty()) {
                    var minHeightOfLiterals = literals.values().stream().mapToInt(Node::getMinHeight).min().orElse(Integer.MAX_VALUE);
                    var minHeightOfPatterns = patterns.values().stream().mapToInt(pat -> pat.node.getMinHeight()).min().orElse(Integer.MAX_VALUE);
                    minHeight = Optional.of(1 + Math.min(minHeightOfLiterals, minHeightOfPatterns));
                }
                return minHeight.get();
            }

            @Override
            public void add(List<Pattern> patternRow, Expr expr) {
                if (patternRow.size() == 0) {
                    System.err.println("This shouldn't ever happen(:");
                    return;
                }

                var first = patternRow.get(0);
                var rest = patternRow.subList(1, patternRow.size());

                var node = patternRow.size() == 1 ? new Leaf(expr) : new Internal();

                if (isLiteral(first)) {
                    var lit = toLiteral(first);
                    node = literals.getOrDefault(lit, node);
                    node.add(rest, expr);
                    literals.put(lit, node);
                } else {
                    var key = new AstPrinter().print(first);
                    var pat = patterns.getOrDefault(key, new PatternMapping(first, node));
                    pat.node.add(rest, expr);
                    patterns.put(key, pat);
                }
            }

            @Override
            public String toString() {
                return prettyPrint(0);
            }
        }

        public static class Leaf extends Node {
            public final Expr expr;

            public Leaf(Expr expr) {
                this.expr = expr;
            }

            @Override
            public int getMinHeight() {
                return 0;
            }

            @Override
            public void add(List<Pattern> patterns, Expr expr) {}

            @Override
            public Optional<Resolution> resolve(Interpreter interpreter, List<Value> args, Environment env) {
                return Optional.of(new Resolution(this, env, args));
            }

            @Override
            public String prettyPrint(int depth) {
                return toString();
            }

            @Override
            public String toString() {
                return new AstPrinter().print(expr);
            }
        }
    }
}
*/