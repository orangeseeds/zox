const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
};

pub const Literal = union(enum) {
    String: []const u8,
    Number: f64,
    True,
    False,
    Nil,

    pub fn to_string(self: Literal, allocator: Allocator) ![]const u8 {
        return switch (self) {
            .True => "true",
            .False => "false",
            .Nil => "nil",
            .String => |str| str,
            .Number => |num| {
                return try fmt.allocPrint(allocator, "{d}", .{num});
            },
        };
    }
};

pub const Token = struct {
    const Self = @This();
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
    line_num: u32,
    col: u32,

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: ?Literal, line_num: u32, col: u32) Token {
        return Token{
            .token_type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line_num = line_num,
            .col = col,
        };
    }

    pub fn to_string(self: Self, allocator: Allocator) ![]const u8 {
        if (self.literal) |lit| {
            return try fmt.allocPrint(allocator, "{} {s} {s}", .{ self.token_type, self.lexeme, try lit.to_string(allocator) });
        }
        return try fmt.allocPrint(allocator, "{} {s}", .{ self.token_type, self.lexeme });
    }
};

pub const Error = error{
    UnexpectedCharacter,
    InvalidNumber,
    UnterminatedString,
};

pub const Scanner = struct {
    const Self = @This();
    const ErrorDetail = struct {
        line: u32,
        col: u32,
        err: Error,
    };
    has_error: bool = false,
    start: u32 = 0,
    current: u32 = 0,
    line: u32 = 1,
    col: u32 = 0,
    input: []const u8,
    allocator: Allocator,
    tokens: std.ArrayList(Token),
    errors: std.ArrayList(ErrorDetail),
    keywords: std.StringArrayHashMap(TokenType),

    pub fn init(allocator: Allocator, input: []const u8) !Scanner {
        var keywords = std.StringArrayHashMap(TokenType).init(allocator);
        try keywords.put("and", .AND);
        try keywords.put("class", .CLASS);
        try keywords.put("else", .ELSE);
        try keywords.put("false", .FALSE);
        try keywords.put("for", .FOR);
        try keywords.put("fun", .FUN);
        try keywords.put("if", .IF);
        try keywords.put("nil", .NIL);
        try keywords.put("or", .OR);
        try keywords.put("print", .PRINT);
        try keywords.put("return", .RETURN);
        try keywords.put("super", .SUPER);
        try keywords.put("this", .THIS);
        try keywords.put("true", .TRUE);
        try keywords.put("var", .VAR);
        try keywords.put("while", .WHILE);
        return Scanner{
            .input = input,
            .allocator = allocator,
            .keywords = keywords,
            .tokens = std.ArrayList(Token).init(allocator),
            .errors = std.ArrayList(ErrorDetail).init(allocator),
        };
    }

    fn is_digit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn is_alpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or char == '_';
    }

    fn is_alphanumeric(char: u8) bool {
        return is_alpha(char) or is_digit(char);
    }

    fn add_error(self: *Self, err: Error) !void {
        self.has_error = true;
        try self.errors.append(ErrorDetail{ .line = self.line, .col = self.col, .err = err });
    }

    fn at_end(self: Self) bool {
        return self.current >= self.input.len;
    }

    /// peeks at the current character
    fn peek(self: Self) u8 {
        if (self.at_end()) return 0;
        return self.input[self.current];
    }

    /// peeks at the next character
    fn peek_next(self: Self) u8 {
        if (self.current + 1 >= self.input.len) return 0;
        return self.input[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.at_end()) return false;
        if (self.input[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    fn advance(self: *Self) u8 {
        const char = self.input[self.current];
        self.current += 1;
        self.col += 1;
        return char;
    }

    fn add_token_with_literal(self: *Self, token_type: TokenType, literal: ?Literal) !void {
        const text = self.input[self.start..self.current];
        try self.tokens.append(Token.init(token_type, text, literal, self.line, self.col));
    }

    fn add_token(self: *Self, token_type: TokenType) !void {
        try self.add_token_with_literal(token_type, null);
    }

    fn string(self: *Self) !void {
        while (self.peek() != '"' and !self.at_end()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }
        if (self.at_end()) {
            // Add lexer error "unterminated string"
            try self.add_error(error.UnterminatedString);
            return;
        }
        _ = self.advance(); // while stops at a char before ", so we shift one position forward
        try self.add_token_with_literal(.STRING, .{ .String = self.input[self.start + 1 .. self.current - 1] });
    }

    fn number(self: *Self) !void {
        while (is_digit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and is_digit(self.peek_next())) {
            _ = self.advance();
            while (is_digit(self.peek())) _ = self.advance();
        }

        const parsed_float = fmt.parseFloat(f16, self.input[self.start..self.current]) catch {
            // add parse error
            try self.add_error(error.InvalidNumber);
            return;
        };
        try self.add_token_with_literal(.NUMBER, .{
            .Number = parsed_float,
        });
    }

    fn identifier(self: *Self) !void {
        while (is_alphanumeric(self.peek())) _ = self.advance();
        const text = self.input[self.start..self.current];
        if (self.keywords.get(text)) |keyword_token| {
            try self.add_token(keyword_token);
        } else {
            try self.add_token(.IDENTIFIER);
        }
    }

    fn scan_token(self: *Self) !void {
        const char: u8 = self.advance();
        switch (char) {
            '(' => try self.add_token(.LEFT_PAREN),
            ')' => try self.add_token(.RIGHT_PAREN),
            '{' => try self.add_token(.LEFT_BRACE),
            '}' => try self.add_token(.RIGHT_BRACE),
            ',' => try self.add_token(.COMMA),
            '.' => try self.add_token(.DOT),
            '-' => try self.add_token(.MINUS),
            '+' => try self.add_token(.PLUS),
            ';' => try self.add_token(.SEMICOLON),
            '*' => try self.add_token(.STAR),
            '!' => try self.add_token(if (self.match('=')) .BANG_EQUAL else .BANG), // if current == "="
            '=' => try self.add_token(if (self.match('=')) .EQUAL_EQUAL else .EQUAL),
            '>' => try self.add_token(if (self.match('=')) .GREATER_EQUAL else .GREATER),
            '<' => try self.add_token(if (self.match('=')) .LESS_EQUAL else .LESS),
            '/' => { // scanning comment strings
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.at_end()) _ = self.advance();
                } else {
                    try self.add_token(.SLASH);
                }
            },
            ' ' => {},
            '\r' => {},
            '\t' => {},
            '\n' => {
                self.col = 0;
                self.line += 1;
            },
            '"' => try self.string(),
            else => {
                if (is_digit(char)) {
                    try self.number();
                    return;
                }

                if (is_alpha(char)) {
                    try self.identifier();
                    return;
                }

                try self.add_error(error.UnexpectedCharacter);
            },
        }
    }

    pub fn scan_tokens(self: *Self) !void {
        while (!self.at_end()) {
            self.start = self.current; // start is changing for reading multi-char lexeme
            try self.scan_token(); // NOTE: OutOfMemory error returns, other scanner error add to error list.
        }
        try self.add_token(.EOF);
    }

    pub fn deinit(self: *Self) void {
        self.keywords.deinit();
        self.tokens.deinit();
        self.errors.deinit();
    }
};

test "Test Scanner" {
    const input =
        \\ five = 5;
        \\ ten = 10;
        \\ add = fun(x, y) {
        \\ 	x + y;
        \\ };
        \\ result = add(five, ten);
        \\ 
        \\ !-*/5;
        \\ 5 < 10 > 5;
        \\ 
        \\ if (5 < 10) {
        \\ 	return true;
        \\ } else {
        \\ 	return false;
        \\ }
        \\ 
        \\ 10 == 10;
        \\ 10 != 9;
        \\ 
        \\ "foobar"
        \\ "foo bar"
        \\ 
        \\ {1, 2};
        \\ 
        \\ {"foo", "bar"}
        \\ 
        \\ true and false;
        \\ true or false;
        \\ 
        \\ \// This is a comment..
        \\ 10
        \\ 
        \\ 10
        \\ 
        \\ cantChangeMe = "neato";
        \\ 
        \\ 5 >= 5;
        \\ 5 <= 5;
        \\ 
        \\ snake_case_with_question_mark = true;
    ;
    const expected = [_]TokenType{
        .IDENTIFIER,
        .EQUAL,
        .NUMBER,
        .SEMICOLON,

        .IDENTIFIER,
        .EQUAL,
        .NUMBER,
        .SEMICOLON,

        .IDENTIFIER,
        .EQUAL,
        .FUN,
        .LEFT_PAREN,
        .IDENTIFIER,
        .COMMA,
        .IDENTIFIER,
        .RIGHT_PAREN,
        .LEFT_BRACE,
        .IDENTIFIER,
        .PLUS,
        .IDENTIFIER,
        .SEMICOLON,
        .RIGHT_BRACE,
        .SEMICOLON,

        .IDENTIFIER,
        .EQUAL,
        .IDENTIFIER,
        .LEFT_PAREN,
        .IDENTIFIER,
        .COMMA,
        .IDENTIFIER,
        .RIGHT_PAREN,
        .SEMICOLON,

        .BANG,
        .MINUS,
        .STAR,
        .SLASH,
        .NUMBER,
        .SEMICOLON,

        .NUMBER,
        .LESS,
        .NUMBER,
        .GREATER,
        .NUMBER,
        .SEMICOLON,

        .IF,
        .LEFT_PAREN,
        .NUMBER,
        .LESS,
        .NUMBER,
        .RIGHT_PAREN,
        .LEFT_BRACE,
        .RETURN,
        .TRUE,
        .SEMICOLON,
        .RIGHT_BRACE,
        .ELSE,
        .LEFT_BRACE,
        .RETURN,
        .FALSE,
        .SEMICOLON,
        .RIGHT_BRACE,

        .NUMBER,
        .EQUAL_EQUAL,
        .NUMBER,
        .SEMICOLON,

        .NUMBER,
        .BANG_EQUAL,
        .NUMBER,
        .SEMICOLON,

        .STRING,
        .STRING,

        .LEFT_BRACE,
        .NUMBER,
        .COMMA,
        .NUMBER,
        .RIGHT_BRACE,
        .SEMICOLON,

        .LEFT_BRACE,
        .STRING,
        .COMMA,
        .STRING,
        .RIGHT_BRACE,

        .TRUE,
        .AND,
        .FALSE,
        .SEMICOLON,

        .TRUE,
        .OR,
        .FALSE,
        .SEMICOLON,

        .NUMBER,

        .NUMBER,

        .IDENTIFIER,
        .EQUAL,
        .STRING,
        .SEMICOLON,

        .NUMBER,
        .GREATER_EQUAL,
        .NUMBER,
        .SEMICOLON,

        .NUMBER,
        .LESS_EQUAL,
        .NUMBER,
        .SEMICOLON,

        .IDENTIFIER,
        .EQUAL,
        .TRUE,
        .SEMICOLON,
        .EOF,
    };

    var scanner = try Scanner.init(std.testing.allocator, input);
    defer scanner.deinit();

    try scanner.scan_tokens();

    for (scanner.tokens.items, 0..) |token, i| {
        try expect(token.token_type == expected[i]);
    }
    // var spaces: [100]u8 = undefined;
    // @memset(&spaces, ' ');
    // var seq = std.mem.splitSequence(u8, scanner.input, "\n");
    // if (scanner.has_error) for (scanner.errors.items) |err| {
    //     std.debug.print("Error: {s}\n\n", .{err.message});
    //     std.debug.print("\t{d} | {s}\n", .{ err.line, seq.first()[0..err.col] });
    //     std.debug.print("\t    {s}^--Here\n", .{spaces[0 .. err.col - 1]});
    // };

}

test "Scanner Errors" {
    // Unexpected Character
    // Unterminated String
    // Invalid Number
    const input =
        \\ a ? b
        \\ "apple
    ;

    const errors = [_]Error{
        Error.UnexpectedCharacter,
        Error.UnterminatedString,
    };

    var scanner = try Scanner.init(std.testing.allocator, input);
    defer scanner.deinit();

    try scanner.scan_tokens();

    try std.testing.expect(scanner.has_error);

    for (scanner.errors.items, 0..) |err, i| {
        try std.testing.expect(err.err == errors[i]);
    }
}
