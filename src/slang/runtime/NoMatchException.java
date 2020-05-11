package slang.runtime;

public class NoMatchException extends RuntimeException {
    public NoMatchException() {
        super("Matchbox failed all matches.");
    }
}
