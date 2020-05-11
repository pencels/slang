package slang.parse;

public class AbbreviatedAstPrinter extends AstPrinter {
    @Override
    public String visitBlockExpr(Expr.Block block) {
        if (block.statements.size() == 1) {
            return "{ " + print(block.statements.get(0)) + " }";
        }
        return "{ ... }";
    }
}
