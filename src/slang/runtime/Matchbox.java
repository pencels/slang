package slang.runtime;

import slang.analysis.UsedBindings;
import slang.parse.*;
import slang.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// public class Matchbox extends Value {
//     public final List<Stmt.Match> matches;
//     public final Environment environment;
//
//     public Matchbox(List<Stmt.Match> matches, Environment environment) {
//         this(matches, environment, false);
//     }
//
//     public Matchbox(List<Stmt.Match> matches, Environment environment, boolean isBase) {
//         this.matches = matches;
//         this.environment = new Environment(environment);
//         if (isBase) {
//             // Only define self if this is the base matchbox.
//             this.environment.define("self", this);
//         }
//     }
//
//     public Value apply(Interpreter interpreter, List<Value> args) {
//         Stack<Value> argPrefix = new Stack<>();
//         List<Pair<Stmt.Match, Environment>> possibleMatches = matches.stream().map(m -> new Pair<>(m, (Environment) null)).collect(Collectors.toList());
//
//         // Eval incrementally by argument. This means only eval arguments
//         for (int i = 0; i < args.size(); i++) {
//             Value arg = args.get(i);
//
//             final int finalI = i;
//             boolean needEvalLazy = possibleMatches
//                     .stream()
//                     .filter(pair -> pair.left.patterns.size() >= args.size()) // Only compare with patterns that make sense
//                     .map(pair -> pair.left.patterns.get(finalI))
//                     .anyMatch(pattern -> pattern instanceof Pattern.Literal || pattern instanceof Pattern.Seq);
//             if (arg instanceof Lazy && needEvalLazy) {
//                 // Eval lazy for match. Lazy now keeps a cache of the value.
//                 ((Lazy) arg).getValue(interpreter);
//             }
//
//             // Add eval'd arg to prefix.
//             argPrefix.push(arg);
//
//             // Check prefix against all rows
//             List<Pair<Stmt.Match, Environment>> nextMatches = new ArrayList<>();
//             for (Pair<Stmt.Match, Environment> pair : possibleMatches) {
//                 Stmt.Match match = pair.left;
//                 Environment bindings = interpreter.destructureAssignment(match.patterns, argPrefix, environment);
//
//                 if (bindings == null) continue; // skip fails
//
//                 nextMatches.add(new Pair<>(match, bindings));
//             }
//             possibleMatches = nextMatches;
//         }
//
//         if (possibleMatches.isEmpty()) {
//             throw new NoMatchException();
//         }
//
//         // The first possible match is the one taken. Use the relative lengths of args vs patterns to determine currying or another application.
//         Pair<Stmt.Match, Environment> pair = possibleMatches.get(0);
//         Stmt.Match invokedMatch = pair.left;
//         Environment newBindings = pair.right;
//
//         List<Stmt.Match> filteredMatches = possibleMatches
//                 .stream()
//                 .map(m -> m.left)
//                 .collect(Collectors.toList());
//
//         if (invokedMatch.patterns.size() > args.size()) {
//             // Curry opportunity.
//             List<Pattern> strictPrefix = longestStrictPrefix(possibleMatches, args.size());
//
//             // Curried arguments will possibly involve truncation. This means that `self` needs to be preserved as
//             // referring to the true self (top-level), but with the environment of the current matchbox.
//
//             // Make sure prefix exists.
//             if (!strictPrefix.isEmpty()) {
//                 // For each pattern in the strict prefix, destructure it with its corresponding arg.
//                 newBindings = interpreter.destructureAssignment(strictPrefix,
//                         args.subList(0, strictPrefix.size()), environment);
//
//                 List<Stmt.Match> truncatedMatches = filteredMatches
//                         .stream()
//                         .map(m -> new Stmt.Match(m.patterns.subList(strictPrefix.size(), m.patterns.size()), m.expr))
//                         .collect(Collectors.toList());
//
//                 if (strictPrefix.size() == args.size()) {
//                     // The prefix satisfies all the args!!! No deferral needed.
//                     return new Matchbox(truncatedMatches, newBindings);
//                 } else {
//                     // There are still unbound args that can not be bound unambiguously. They must be deferred.
//                     return new DeferredMatchbox(truncatedMatches, newBindings,
//                             args.subList(strictPrefix.size(), args.size()));
//                 }
//             }
//
//             // Defer what could not be bound.
//             return new DeferredMatchbox(
//                     filteredMatches,
//                     environment, // Don't accept new bindings
//                     args
//             );
//         } else if (invokedMatch.patterns.size() < args.size()) {
//             // Extra application.
//             // Apply prefix of args to patterns, then apply rest to result.
//             Object result = interpreter.eval(invokedMatch.expr, newBindings);
//
//             // Force eval on lazies that are in the receiver position.
//             if (result instanceof Lazy) {
//                 result = ((Lazy) result).getValue(interpreter);
//             }
//
//             if (result instanceof Matchbox) {
//                 return ((Matchbox) result).apply(interpreter, args.subList(invokedMatch.patterns.size(), args.size()));
//             } else {
//                 throw new RuntimeError(null, "Cannot apply non-function.");
//             }
//         } else {
//             // Exactly equal args. Just apply.
//             Value result = interpreter.eval(invokedMatch.expr, newBindings);
//             return result;
//         }
//
//     }
//
//     private static boolean patternStrictEquals(Pattern left, Pattern right) {
//         if (left instanceof Pattern.Id && right instanceof Pattern.Id) {
//             return ((Pattern.Id) left).id.lexeme.equals(((Pattern.Id) right).id.lexeme);
//         }
//
//         if (left instanceof Pattern.Ignore && right instanceof Pattern.Ignore) {
//             return true;
//         }
//
//         if (left instanceof Pattern.Literal && right instanceof Pattern.Literal) {
//             return ((Pattern.Literal) left).literal.value.equals(((Pattern.Literal) right).literal.value);
//         }
//
//         /*
//         if (pattern instanceof Pattern.Seq) {
//             List<Pattern> patterns = ((Pattern.Seq) pattern).patterns;
//             // TODO: when i actually have a list object representation
//             return false;
//         }
//
//         if (pattern instanceof Pattern.Spread) {
//
//             // TODO: when i actually have list object representation
//             env.define(((Pattern.Spread) pattern).id.lexeme, values.subList(i, values.size()));
//             return false;
//         }
//          */
//         return false;
//     }
//
//     // Loose equals. Would these patterns match the same thing?
//     private static boolean patternEquals(Pattern left, Pattern right) {
//         if (left instanceof Pattern.Id || right instanceof Pattern.Id) return true;
//         if (left instanceof Pattern.Ignore || right instanceof Pattern.Ignore) return true;
//
//         return patternStrictEquals(left, right);
//     }
//
//     // Is the `prefix` an equivalent pattern to the corresponding prefix of `patterns`?
//     private static boolean patternListIsPrefix(List<Pattern> prefix, List<Pattern> patterns) {
//         for (int i = 0; i < prefix.size(); i++) {
//             if (!patternEquals(prefix.get(i), patterns.get(i))) return false;
//         }
//         return true;
//     }
//
//     private static boolean patternListIsPrefixStrict(List<Pattern> prefix, List<Pattern> patterns) {
//         for (int i = 0; i < prefix.size(); i++) {
//             if (!patternStrictEquals(prefix.get(i), patterns.get(i))) return false;
//         }
//         return true;
//     }
//
//     private static List<Pattern> longestStrictPrefix(List<Pair<Stmt.Match, Environment>> destructurings, int max) {
//         for (; max > 0; max--) {
//             final int finalMax = max;
//             Pair<Stmt.Match, Environment> destructure = destructurings.get(0);
//             List<Pattern> prefix = destructure.left.patterns.subList(0, finalMax);
//
//             boolean allMatch = destructurings
//                     .stream()
//                     .map(pair -> pair.left.patterns.subList(0, finalMax))
//                     .allMatch(otherPatterns -> patternListIsPrefixStrict(prefix, otherPatterns));
//
//             if (allMatch) return prefix;
//         }
//         return new ArrayList<>();
//     }
//
//
//     @Override
//     public String toString() {
//         String repr = environment.collapsedString();
//         repr += " ";
//         repr += new AbbreviatedAstPrinter().print(new Expr.MatchBlock(matches));
//         return repr;
//     }
//
//     @Override
//     public String getType() {
//         return "Matchbox";
//     }
//
//     @Override
//     public String asString() {
//         return "<Matchbox>";
//     }
// }
