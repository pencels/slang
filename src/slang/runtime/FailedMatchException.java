package slang.runtime;

import slang.parse.AstPrinter;
import slang.parse.Stmt;

public class FailedMatchException extends Throwable {
    public FailedMatchException(Stmt.Match match, Object value) {
        super("Could not match value: " + value.toString() + " to pattern: " + new AstPrinter().print(match));
    }
}
