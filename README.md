A simple interpreter using Zig

### Parts
1. Tokenizer & Lexer        -> [link](./src/scanner.zig)
2. Parser                   -> [link](./src/parser.zig)
3. Evaluator & Interpreter  -> [link](./src/interpreter.zig)


### Grammar
expression  ->  binary | unary | group | literal
binary      ->  expression operator expression
unary       ->  ( "-" | "!" ) expression
literal     ->  NUMBER | STRING | "true" | "false" | "nil"
operator    ->  "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/"

