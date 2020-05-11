import sys

def define_ast(output_dir, base_name, types):
    with open(f'{output_dir}/{base_name}.java', 'w') as file:
        file.write('package slang.parse;\n\n')
        file.write('import java.util.List;\n')
        file.write('import slang.lex.*;\n')
        file.write('import slang.parse.*;\n')
        file.write('import slang.runtime.*;\n')
        file.write('\n')
        file.write(f'public abstract class {base_name} {{\n')

        define_visitor(file, base_name, types)

        for class_name, fields in types:
            define_type(file, base_name, class_name, fields)

        # Base accept() method
        file.write('\n  public abstract <R> R accept(Visitor<R> visitor);\n')

        file.write('}\n')

def define_type(file, base_name, class_name, fields):
    file.write(f'  public static class {class_name} extends {base_name} {{\n')
    file.write(f'    public {class_name}({", ".join(fields)}) {{\n')

    for field in fields:
        ty, name = field.split(' ')
        file.write(f'      this.{name} = {name};\n')

    file.write('    }\n\n')

    file.write('    public <R> R accept(Visitor<R> visitor) {\n')
    file.write(f'      return visitor.visit{class_name}{base_name}(this);\n')
    file.write('    }\n\n')

    for field in fields:
        file.write(f'    public final {field};\n')

    file.write('  }\n')

def define_visitor(file, base_name, types):
    file.write('  public interface Visitor<R> {\n')

    for type, _ in types:
        file.write(f'    R visit{type}{base_name}({type} {base_name.lower()});\n')

    file.write('  }\n')

if len(sys.argv) != 2:
    print(f'Usage: {sys.argv[0]} <output directory>', file=sys.stderr)
    sys.exit(1)

output_dir = sys.argv[1]

define_ast(output_dir, 'Expr', [
    ('Assign', ['Pattern left', 'Expr right']),
    ('Binary', ['Expr left', 'Token op', 'Expr right']),
    ('Block', ['List<Stmt> statements']),
    ('MatchBlock', ['List<Stmt.Match> matches']),
    ('Call', ['Expr left', 'List<Expr> args']),
    ('Grouping', ['Expr expr']),
    ('Id', ['String id']),
    ('Literal', ['Value value']),
    ('Postfix', ['Token op', 'Expr expr']),
    ('Seq', ['List<Expr> elements']),
    ('Unary', ['Token op', 'Expr expr']),
])

define_ast(output_dir, 'Pattern', [
    ('Id', ['Token id']),
    ('Ignore', ['Token ignore']),
    ('Lazy', ['Pattern inner']),
    ('Literal', ['Token literal', 'Value value']),
    ('Seq', ['List<Pattern> patterns']),
    ('Spread', ['Token id']),
])

define_ast(output_dir, 'Stmt', [
    ('Expression', ['Expr expr']),
    ('Let', ['Pattern pattern', 'Expr init']),
    ('Print', ['Expr expr']),
    ('Match', ['List<Pattern> patterns', 'Expr expr']),
])
