package slang

import slang.lex.Lexer
import slang.lex.Token
import slang.parse._
import slang.runtime.Environment
import slang.runtime.Interpreter
import slang.runtime.SlangNothing
import slang.runtime.RuntimeError
import slang.runtime.Value
import slang.runtime.NativeFunction
import slang.runtime.Lazy
import slang.runtime.Matchbox
import slang.runtime.SlangString
import slang.runtime.SlangList

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

        loadBuiltins(rootEnv)

        try {
            var prelude = load("lib/prelude.slang")
            interpret(rootEnv, prelude)
        } catch {
            case e: IOException => System.err.println("Error: could not open 'lib/prelude.slang'")
        }

        printPrompt()
        if (input.hasNextLine()) {
            repl(input, rootEnv)
        }
    }

    @tailrec
    final def repl(input: Scanner, rootEnv: Environment): Unit = {
        var text = input.nextLine().trim()

        text += "\n" // Re-append a newline to make the parser happy.

        if (text.startsWith(".eg")) {
            text = loadExample(text.substring(".eg".length).trim())
            interpret(rootEnv, text)
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
            interpret(rootEnv, text)
        }

        printPrompt()

        if (input.hasNextLine()) {
            repl(input, rootEnv)
        }
    }

    def interpret(rootEnv: Environment, text: String): Unit = {
        val lexer = new Lexer(text)
        val parser = new Parser(lexer)

        try {
            var value: Value = SlangNothing
            for (expr <- parser) {
                value = Interpreter.strictEval(rootEnv, expr, true)

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
    
    def loadBuiltins(env: Environment) = {
        env.define("__to_num__", NativeFunction({
            case n :: rest => (slang.runtime.Number(n.tryAsDouble(null)), rest)
            case _ => throw new RuntimeError(null, "__to_num__ expects one argument")
        }))

        env.define("__neg__", NativeFunction({
            case n :: rest => (slang.runtime.Number(-n.tryAsDouble(null)), rest)
            case _ => throw new RuntimeError(null, "__neg__ expects one argument")
        }))

        env.define("__strict__", NativeFunction({
            case v :: rest => (Interpreter.strictCoerce(v), rest)
            case _ => throw new RuntimeError(null, "__strict__ expects one argument")
        }))

        env.define("__full_strict__", NativeFunction({
            case v :: rest => (Interpreter.strictCoerce(v, full = true), rest)
            case _ => throw new RuntimeError(null, "__full_strict__ expects one argument")
        }))

        env.define("__lt__", NativeFunction({
            case l :: r :: rest =>
                val truth = l.tryAsDouble(null) < r.tryAsDouble(null)
                val value = if (truth) Interpreter.TRUE_ATOM else Interpreter.FALSE_ATOM
                (value, rest)
            case _ => throw new RuntimeError(null, "__lt__ expects two arguments")
        }))

        env.define("__le__", NativeFunction({
            case l :: r :: rest =>
                val truth = l.tryAsDouble(null) <= r.tryAsDouble(null)
                val value = if (truth) Interpreter.TRUE_ATOM else Interpreter.FALSE_ATOM
                (value, rest)
            case _ => throw new RuntimeError(null, "__le__ expects two arguments")
        }))

        env.define("__gt__", NativeFunction({
            case l :: r :: rest =>
                val truth = l.tryAsDouble(null) > r.tryAsDouble(null)
                val value = if (truth) Interpreter.TRUE_ATOM else Interpreter.FALSE_ATOM
                (value, rest)
            case _ => throw new RuntimeError(null, "__gt__ expects two arguments")
        }))

        env.define("__ge__", NativeFunction({
            case l :: r :: rest =>
                val truth = l.tryAsDouble(null) >= r.tryAsDouble(null)
                val value = if (truth) Interpreter.TRUE_ATOM else Interpreter.FALSE_ATOM
                (value, rest)
            case _ => throw new RuntimeError(null, "__ge__ expects two arguments")
        }))

        env.define("__eq__", NativeFunction({
            case l :: r :: rest =>
                val truth = l == r
                val value = if (truth) Interpreter.TRUE_ATOM else Interpreter.FALSE_ATOM
                (value, rest)
            case _ => throw new RuntimeError(null, "__eq__ expects two arguments")
        }))

        env.define("__ne__", NativeFunction({
            case l :: r :: rest =>
                val truth = l != r
                val value = if (truth) Interpreter.TRUE_ATOM else Interpreter.FALSE_ATOM
                (value, rest)
            case _ => throw new RuntimeError(null, "__ne__ expects two arguments")
        }))

        env.define("__add__", NativeFunction({
            case l :: r :: rest =>
                val value = slang.runtime.Number(l.tryAsDouble(null) + r.tryAsDouble(null))
                (value, rest)
            case _ => throw new RuntimeError(null, "__add__ expects two Number arguments")
        }))

        env.define("__sub__", NativeFunction({
            case l :: r :: rest =>
                val value = slang.runtime.Number(l.tryAsDouble(null) - r.tryAsDouble(null))
                (value, rest)
            case _ => throw new RuntimeError(null, "__sub__ expects two Number arguments")
        }))

        env.define("__mul__", NativeFunction({
            case l :: r :: rest =>
                val value = slang.runtime.Number(l.tryAsDouble(null) * r.tryAsDouble(null))
                (value, rest)
            case _ => throw new RuntimeError(null, "__mul__ expects two Number arguments")
        }))

        env.define("__div__", NativeFunction({
            case l :: r :: rest =>
                val value = slang.runtime.Number(l.tryAsDouble(null) / r.tryAsDouble(null))
                (value, rest)
            case _ => throw new RuntimeError(null, "__div__ expects two Number arguments")
        }))

        env.define("__mod__", NativeFunction({
            case l :: r :: rest =>
                val value = slang.runtime.Number(l.tryAsDouble(null) % r.tryAsDouble(null))
                (value, rest)
            case _ => throw new RuntimeError(null, "__mod__ expects two Number arguments")
        }))

        env.define("__concat_str__", NativeFunction({
            case SlangString(l) :: r :: rest => (SlangString(l + r.toSlangString), rest)
            case _ => throw new RuntimeError(null, "__concat_str__ expects two String arguments")
        }))

        env.define("__concat_list__", NativeFunction({
            case SlangList(l) :: SlangList(r) :: rest => (SlangList(l ++ r), rest)
            case _ => throw new RuntimeError(null, "__concat_list__ expects two List arguments")
        }))
    }
}
