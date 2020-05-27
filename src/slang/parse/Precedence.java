package slang.parse;

public class Precedence {
    public static final int SEQUENCE = 1;
    public static final int ASSIGNMENT = 2;
    public static final int APPLY = 3;
    public static final int CONDITIONAL = 4;
    public static final int SUM = 5;
    public static final int PRODUCT = 6;
    public static final int EXPONENT = 7;
    public static final int PREFIX = 8;
    public static final int POSTFIX = 9;
    public static final int CALL = 10;
}
