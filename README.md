A simple interpreter using Zig

### Parts
1. Tokenizer & Lexer        → [link](./src/scanner.zig)
2. Parser                   → [link](./src/parser.zig)
3. Evaluator & Interpreter  → [link](./src/interpreter.zig)


### Grammar
expression  →  binary | unary | group | literal
binary      →  expression operator expression
unary       →  ( "-" | "!" ) expression
literal     →  NUMBER | STRING | "true" | "false" | "nil"
operator    →  "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/"


### Statements
statement → exprStmt
          | ifStmt
          | printStmt
          | whileStmt
          | block ;

whileStmt → "while" "(" expression ")" statement ;

primary        → "true" | "false" | "nil"
               | NUMBER | STRING
               | "(" expression ")"
               | IDENTIFIER ;


program        → declaration* EOF ;

varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
declaration    → varDecl
               | statement ;


