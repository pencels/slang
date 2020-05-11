package slang.runtime;

import slang.parse.AbbreviatedAstPrinter;
import slang.parse.Expr;
import slang.parse.Stmt;

import java.util.ArrayList;
import java.util.List;

public class DeferredMatchbox extends Matchbox {
    public List<Value> deferredArgs;

    public DeferredMatchbox(List<Stmt.Match> matches, Environment environment, List<Value> deferredArgs) {
        super(matches, environment);
        this.deferredArgs = deferredArgs;
    }

    @Override
    public Value apply(Interpreter interpreter, List<Value> args) {
        List<Value> newArgs = new ArrayList<>(deferredArgs);
        newArgs.addAll(args);
        return super.apply(interpreter, newArgs);
    }

    @Override
    public String toString() {
        String repr = environment.collapsedString();
        repr += " deferring ";
        repr += deferredArgs;
        repr += " ";
        repr += new AbbreviatedAstPrinter().print(new Expr.MatchBlock(matches));
        return repr;
    }
}
