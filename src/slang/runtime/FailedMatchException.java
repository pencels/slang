package slang.runtime;

import slang.parse.AstPrinter;
import slang.parse.Pattern;

public class FailedMatchException extends RuntimeException {
    public FailedMatchException(Pattern pattern, Value value) {
        super("Could not match value: " + value + " to pattern: " + new AstPrinter().print(pattern));
    }
}
