package slang;

import slang.lex.Lexer;
import slang.lex.Token;
import slang.parse.*;
import slang.runtime.Environment;
import slang.runtime.Interpreter;
import slang.runtime.SlangNothing;
import slang.runtime.RuntimeError;
import slang.runtime.Value;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Scanner;

/**
 * @author chris
 */
public class Slang {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        Environment rootEnv = new Environment();
        Interpreter interpreter = new Interpreter();

        try {
            var prelude = load("lib/prelude.slang");
            interpret(interpreter, rootEnv, prelude);
        } catch (IOException e) {
            System.err.println("Error: could not open 'lib/prelude.slang'");
        }

        printPrompt();
        for (; input.hasNextLine(); printPrompt()) {
            String text = input.nextLine().trim();

            if (text.isEmpty()) continue;

            text += "\n";

            if (text.startsWith(".eg")) {
                Scanner cmd = new Scanner(text);
                cmd.next();

                String filename = cmd.hasNext() ? cmd.next() : "";

                try {
                    text = loadExample(filename);
                } catch (IOException e) {
                    System.err.format("Error: could not open 'examples/%s'\n", filename);
                    continue;
                }
            }

            if (text.startsWith(".lex")) {
                Scanner cmd = new Scanner(text);
                cmd.next();

                String program = cmd.hasNextLine() ? cmd.nextLine() : "";

                Lexer lexer = new Lexer(program);
                List<Token> tokens = lexer.lex();

                for (Token token : tokens) {
                    System.out.println(token);
                }

                continue;
            }

            if (text.startsWith(".parse")) {
                Scanner cmd = new Scanner(text);
                cmd.next();

                String program = cmd.hasNextLine() ? cmd.nextLine() : "";
                program += "\n";

                Lexer lexer = new Lexer(program);
                List<Token> tokens = lexer.lex();
                Parser parser = new Parser(tokens);

                for (Expr expr : parser.parse()) {
                    System.out.println(new AstPrinter().print(expr));
                }

                continue;
            }

            interpret(interpreter, rootEnv, text);
        }
    }

    private static void interpret(Interpreter interpreter, Environment rootEnv, String text) {
        Lexer lexer = new Lexer(text);
        List<Token> tokens = lexer.lex();
        Parser parser = new Parser(tokens);

        try {
            List<Expr> exprs = parser.parse();
            Value value = null;
            for (Expr expr : exprs) {
                value = interpreter.eval(rootEnv, expr);

                if (expr instanceof Expr.Let) {
                    System.out.println(new AstPrinter().print(expr));
                }
            }
            if (value != SlangNothing.getInstance()) {
                System.out.println(new AstPrinter().print(value));
            }
        } catch (Exception error) {
            error.printStackTrace();
        }
    }

    static void printPrompt() {
        System.out.print("> ");
        System.out.flush();
    }

    static String loadExample(String filename) throws IOException {
        Path path = FileSystems.getDefault().getPath("examples", filename);
        return new String(Files.readAllBytes(path));
    }

    static String load(String filename) throws IOException {
        Path path = FileSystems.getDefault().getPath(filename);
        return new String(Files.readAllBytes(path));
    }

    public static void runtimeError(RuntimeError error) {
        System.err.println("[runtime error] " + error.getMessage());
    }

    public static void parserError(ParseException error) {
        System.err.format("[line %d, col %d] %s\n", error.token.line, error.token.col, error.getMessage());
    }

    public static void lexerError(char c) {
        System.err.format("Unexpected character %c\n", c);
    }
}
