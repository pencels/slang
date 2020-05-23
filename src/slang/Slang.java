package slang;

import slang.lex.Lexer;
import slang.lex.Token;
import slang.parse.*;
import slang.runtime.Environment;
import slang.runtime.Interpreter;
import slang.runtime.SlangNothing;
import slang.runtime.RuntimeError;

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
                    text = load(filename);
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

                for (Stmt stmt : parser.parse()) {
                    System.out.println(new AstPrinter().print(stmt));
                }

                continue;
            }

            Lexer lexer = new Lexer(text);
            List<Token> tokens = lexer.lex();
            Parser parser = new Parser(tokens);

            try {
                List<Stmt> stmts = parser.parse();
                Object value = null;
                for (Stmt stmt : stmts) {
                    value = interpreter.execute(rootEnv, stmt);

                    if (stmt instanceof Stmt.Let) {
                        System.out.println(new AbbreviatedAstPrinter().print(stmt));
                    }
                }
                if (value != SlangNothing.getInstance()) {
                    System.out.println(value.toString());
                }
            } catch (Exception error) {
                error.printStackTrace();
            }
        }
    }

    static void printPrompt() {
        System.out.print("> ");
        System.out.flush();
    }

    static String load(String filename) throws IOException {
        Path path = FileSystems.getDefault().getPath("examples", filename);
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
