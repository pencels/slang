package slang

import slang.lex.Lexer
import slang.lex.Token
import slang.parse._
import slang.runtime.Environment
import slang.runtime.Interpreter
import slang.runtime.SlangNothing
import slang.runtime.RuntimeError
import slang.runtime.Value

import java.io.IOException
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.util.Scanner
import scala.annotation.tailrec

/**
 * @author chris
 */
object Slang {

    /**
     * @param args the command line arguments
     */
    def main(args: Array[String]) = {
        val input = new Scanner(System.in)
        val rootEnv = new Environment
        val interpreter = new Interpreter

        try {
            var prelude = load("lib/prelude.slang")
            interpret(interpreter, rootEnv, prelude)
        } catch {
            case e: IOException => System.err.println("Error: could not open 'lib/prelude.slang'")
        }

        printPrompt()
        if (input.hasNextLine()) {
            repl(input, interpreter, rootEnv)
        }
    }

    @tailrec
    final def repl(input: Scanner, interpreter: Interpreter, rootEnv: Environment): Unit = {
        var text = input.nextLine().trim()

        text += "\n" // Re-append a newline to make the parser happy.

        if (text.startsWith(".eg")) {
            text = loadExample(text.substring(".eg".length).trim())
            interpret(interpreter, rootEnv, text)
        } else if (text.startsWith(".lex_file")) {
            text = load(text.substring(".lex_file".length).trim())
            printTokens(text)
        } else if (text.startsWith(".lex")) {
            printTokens(text.substring(".lex".length).trim())
        } else if (text.startsWith(".parse_file")) {
            text = load(text.substring(".parse_file".length()).trim())
            printAst(text)
        } else if (text.startsWith(".parse")) {
            printAst(text.substring(".parse".length()).trim())
        } else {
            interpret(interpreter, rootEnv, text)
        }

        printPrompt()

        if (input.hasNextLine()) {
            repl(input, interpreter, rootEnv)
        }
    }

    def interpret(interpreter: Interpreter, rootEnv: Environment, text: String): Unit = {
        val lexer = new Lexer(text)
        val parser = new Parser(lexer)

        try {
            var value: Value = SlangNothing
            for (expr <- parser) {
                value = interpreter.strictEval(rootEnv, expr, true)

                if (expr.isInstanceOf[Expr.Let]) {
                    println(new AstPrinter().print(expr))
                }
            }
            if (value != SlangNothing) {
                println(new AstPrinter().print(value))
            }
        } catch {
            case error: Throwable => error.printStackTrace
        }
    }

    def loadExample(filename: String): String = {
        try {
            loadExampleFile(filename)
        } catch {
            case e: IOException =>
                System.err.format("Error: could not open 'examples/%s'\n", filename)
                ""
        }
    }

    def printTokens(program: String) = {
        val lexer = new Lexer(program)
        for (token <- lexer) {
            pprint.pprintln(token)
        }
    }

    def printAst(program: String) = {
        val lexer = new Lexer(program)
        val parser = new Parser(lexer)

        for (expr <- parser) {
            pprint.pprintln(expr)
        }
    }

    def printPrompt() = {
        System.out.print("~ ");
        System.out.flush();
    }

    def loadExampleFile(filename: String): String = {
        val path = FileSystems.getDefault().getPath("examples", filename)
        new String(Files.readAllBytes(path))
    }

    def load(filename: String): String = {
        val path = FileSystems.getDefault().getPath(filename)
        new String(Files.readAllBytes(path))
    }

    def runtimeError(error: RuntimeError) = {
        System.err.println("[runtime error] " + error.getMessage())
    }

    def parserError(error: ParseException) = {
        System.err.format("[line %d, col %d] %s\n", error.token.loc.line, error.token.loc.col, error.getMessage())
    }

    def lexerError(c: Char) = {
        System.err.format("Unexpected character %c\n", c)
    }
}
