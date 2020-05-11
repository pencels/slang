package slang.parse;

import slang.lex.Token;
import slang.runtime.Interpreter;
import slang.runtime.Value;

public class LiteralParselet implements PrefixParselet {
    @Override
    public Expr parse(Parser parser, Token token) {
        return new Expr.Literal(valueFromToken(token));
    }

    public static Value valueFromToken(Token token) {
        Value value = null;
        switch (token.type) {
            case NOTHING:
                value = Value.Nothing.getInstance();
                break;
            case TRUE: // TODO: do we really need overlap between "true" and ":true"???
                value = Interpreter.TRUE_ATOM;
                break;
            case FALSE:
                value = Interpreter.FALSE_ATOM;
                break;
            case ATOM:
                value = Value.Atom.create((String) token.value);
                break;
            default:
                if (token.value instanceof String) {
                    value = new Value.SlangString((String) token.value);
                } else if (token.value instanceof Double) {
                    value = new Value.Double((Double) token.value);
                } else {
                    throw new ParseException(token, "Cannot derive value for literal.");
                }
        }
        return value;
    }
}
