/*
The MIT License (MIT)
Copyright (c) Tymon Wozniak

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include <complex>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

typedef unsigned long size;
typedef unsigned long UnitNumber;
typedef std::variant<UnitNumber, const char*, std::string> UnitValue;

enum class UnitType: unsigned short {
  WHITESPACE = 0,
  NUMBER,
  PLUS,
  MINUS,
  ASTERISK,
  FORWARD_SLASH,
  PAREN_OPEN,
  PAREN_CLOSE,
  END,
  NUMBER_EXPR,
  BINARY_EXPR,
  PAREN_EXPR,
  ERROR
};

std::unordered_map<UnitType, const char*> types = {
  {UnitType::WHITESPACE, "whitespace"},
  {UnitType::NUMBER, "number"},
  {UnitType::PLUS, "plus"},
  {UnitType::MINUS, "minus"},
  {UnitType::ASTERISK, "asterisk"},
  {UnitType::FORWARD_SLASH, "forward slash"},
  {UnitType::PAREN_OPEN, "parenthess open"},
  {UnitType::PAREN_CLOSE, "parenthess close"},
  {UnitType::END, "end"},
  {UnitType::NUMBER_EXPR, "number expression"},
  {UnitType::BINARY_EXPR, "binary expression"},
  {UnitType::PAREN_EXPR, "parenthess expression"},
  {UnitType::ERROR, "error"},
};

template<typename... Args>
std::string format(const std::string& format, Args... args) {
  const int size_s = std::snprintf(nullptr, 0, format.c_str(), args...) + 1;
  if (size_s <= 0) throw std::runtime_error("Error during formatting.");
  const auto size = static_cast<size_t>(size_s);
  const auto buf = std::make_unique<char[]>(size);
  std::snprintf(buf.get(), size, format.c_str(), args...);
  return std::string{buf.get(), buf.get() + size - 1};
}

template<typename... Args>
[[noreturn]] void crash(const char* fmt, Args... args) {
  fprintf(stderr, fmt, args...);
  exit(1);
}

struct Token {
  UnitType type = UnitType::ERROR;
  size position = 0;
  std::string content;
  UnitValue value;

  Token() = default;

  Token(const UnitType type, const size position, std::string content, UnitValue value)
    : type(type), position(position), content(std::move(content)), value(std::move(value)) {
  }

  [[nodiscard]] bool is(const UnitType& type) const {
    return this->type == type;
  }
};

bool is_digit(const char& c) {
  return 48 <= c && c <= 57;
}

UnitNumber char_to_digit(const char& c) {
  return c - 48;
}

bool parse_str_to_number(const std::string& str, UnitNumber* out) {
  size position = 0;
  UnitNumber parsed = 0;
  while (position < str.length()) {
    const char c = str[position++];
    if (!is_digit(c)) return false;
    parsed = parsed * 10 + char_to_digit(c);
  }
  *out = parsed;
  return true;
}

class Lexer {
  const std::string content;
  size position = 0;

  [[nodiscard]] bool is_end() const {
    return position >= content.length();
  }

  [[nodiscard]] char current_char() const {
    if (is_end()) return '\0';
    return content[position];
  }

  void next() {
    position++;
  }

  char consume() {
    next();
    return current_char();
  }

public:
  std::vector<std::string> errors;

  explicit Lexer(std::string content) : content(std::move(content)) {
  }

  Token next_token() {
    if (is_end()) return Token{UnitType::END, position, "\0", nullptr};
    const char current = current_char();
    // whitespace
    if (current == ' ') {
      const size start = position;
      next();
      while (current_char() == ' ') next();
      const size length = position - start;
      const std::string token_content = content.substr(start, length);
      return Token{UnitType::WHITESPACE, start, token_content, nullptr};
    }
    // int
    if (is_digit(current)) {
      const size start = position;
      next();
      while (is_digit(current_char())) next();
      const size length = position - start;
      const std::string number_literal = content.substr(start, length);
      UnitNumber value = 0;
      if (!parse_str_to_number(number_literal, &value))
        errors.push_back(format("ERROR: the number '%s' isn't valid unsigned integer", number_literal.c_str()));
      return Token{UnitType::NUMBER, start, number_literal, value};
    }

    const size start = position++;

    if (current == '+') return Token{UnitType::PLUS, start, "+", nullptr};
    if (current == '-') return Token{UnitType::MINUS, start, "-", nullptr};
    if (current == '*') return Token{UnitType::ASTERISK, start, "*", nullptr};
    if (current == '/') return Token{UnitType::FORWARD_SLASH, start, "/", nullptr};
    if (current == '(') return Token{UnitType::PAREN_OPEN, start, "(", nullptr};
    if (current == ')') return Token{UnitType::PAREN_CLOSE, start, ")", nullptr};

    errors.push_back(format("ERROR: bad input character: '%c' @ %lu", current, start));
    return Token{UnitType::ERROR, start, content.substr(start, 1), nullptr};
  }
};

struct Node {
  const UnitType type = UnitType::ERROR;
};

struct Expression : Node {
  virtual ~Expression() = default;
};

struct NumberExpr final : Expression {
  const UnitType type = UnitType::NUMBER_EXPR;
  Token number;

  explicit NumberExpr(Token number) : number(std::move(number)) {
  }
};

struct BinaryExpr final : Expression {
  const UnitType type = UnitType::BINARY_EXPR;
  std::unique_ptr<Expression> left;
  Token oper;
  std::unique_ptr<Expression> right;

  BinaryExpr(std::unique_ptr<Expression> left, Token op, std::unique_ptr<Expression> right) : left(std::move(left)),
    oper(std::move(op)), right(std::move(right)) {
  }
};

struct ParenExpr final : Expression {
  const UnitType type = UnitType::PAREN_EXPR;
  Token paren_open;
  std::unique_ptr<Expression> expr;
  Token paren_close;

  ParenExpr(Token paren_open, std::unique_ptr<Expression> expression, Token paren_close)
    : paren_open(std::move(paren_open)), expr(std::move(expression)), paren_close(std::move(paren_close)) {
  }
};

struct AST {
  std::vector<std::string> errors;
  std::unique_ptr<Expression> root;
  Token eof;

  AST(const std::vector<std::string>& errors, std::unique_ptr<Expression> root, Token eof) : errors(errors),
    root(std::move(root)), eof(std::move(eof)) {
  }

  static AST parse(const std::string& content);
};

struct Parser {
  std::vector<Token> tokens;
  std::vector<std::string> errors;
  size position = 0;

  explicit Parser(const std::string& content) {
    Lexer lexer{content};
    Token last_token;
    do {
      last_token = lexer.next_token();
      if (!last_token.is(UnitType::WHITESPACE) && !last_token.is(UnitType::ERROR)) tokens.push_back(last_token);
    }
    while (!last_token.is(UnitType::END));
    errors = std::vector(lexer.errors.begin(), lexer.errors.end());
  }

  Token& peek(const size offset) {
    const size index = position + offset;
    if (index >= tokens.size()) return tokens[tokens.size() - 1];
    return tokens[index];
  }

  Token& current() {
    return peek(0);
  }

  Token& next_token() {
    Token& cur = current();
    position += 1;
    return cur;
  }

  Token match(const UnitType& type) {
    const Token& cur = current();
    if (cur.is(type)) {
      return next_token();
    }
    errors.push_back(format("ERROR: unexpected token: <%s>, expected <%s>", types[cur.type], types[type]));
    return Token{type, cur.position, "\0", nullptr};
  }

  std::unique_ptr<Expression> parse_expr() {
    return parse_term();
  }

  std::unique_ptr<Expression> parse_term() {
    std::unique_ptr<Expression> left = parse_factor();
    Token cur = current();
    while (cur.is(UnitType::PLUS) || cur.is(UnitType::MINUS)) {
      const Token op = next_token();
      auto right = parse_factor();
      left = std::unique_ptr<Expression>{new BinaryExpr(std::move(left), op, std::move(right))};
      cur = current();
    }
    return left;
  }

  std::unique_ptr<Expression> parse_primary() {
    if (current().is(UnitType::PAREN_OPEN)) {
      const Token left = next_token();
      auto expr = parse_expr();
      const Token right = match(UnitType::PAREN_CLOSE);
      return std::unique_ptr<Expression>{new ParenExpr(left, std::move(expr), right)};
    }
    const Token number = match(UnitType::NUMBER);
    return std::unique_ptr<Expression>{new NumberExpr(number)};
  }

  std::unique_ptr<Expression> parse_factor() {
    auto left = parse_primary();
    Token cur = current();
    while (cur.is(UnitType::ASTERISK) || cur.is(UnitType::FORWARD_SLASH)) {
      const Token op = next_token();
      auto right = parse_primary();
      left = std::unique_ptr<Expression>{new BinaryExpr(std::move(left), op, std::move(right))};
      cur = current();
    }
    return left;
  }

  AST parse() {
    auto expression = parse_term();
    const Token end = match(UnitType::END);
    return AST{errors, std::move(expression), end};
  }
};

AST AST::parse(const std::string& content) {
  Parser parser{content};
  return parser.parse();
}

struct Evaluator {
  std::unique_ptr<Expression> root;

  explicit Evaluator(std::unique_ptr<Expression> root) : root(std::move(root)) {
  }

  UnitNumber evaluate() {
    return evaluate_expr(root.get());
  }

  UnitNumber evaluate_expr(Expression* expr) {
    // Number Expression
    if (const auto* number_expr = dynamic_cast<NumberExpr*>(expr)) {
      return std::get<size>(number_expr->number.value);
    }
    // Binary expression
    if (const auto* binary_expr = dynamic_cast<BinaryExpr*>(expr)) {
      const UnitNumber left = evaluate_expr(binary_expr->left.get());
      const UnitNumber right = evaluate_expr(binary_expr->right.get());

      if (binary_expr->oper.is(UnitType::PLUS)) return left + right;
      if (binary_expr->oper.is(UnitType::MINUS)) return left - right;
      if (binary_expr->oper.is(UnitType::ASTERISK)) return left * right;
      if (binary_expr->oper.is(UnitType::FORWARD_SLASH)) return left / right;

      crash("unexpected binary operator: %s\n", types[binary_expr->type]);
    }
    // Parentheses expression
    if (const auto* paren_expr = dynamic_cast<ParenExpr*>(expr)) {
      return evaluate_expr(paren_expr->expr.get());
    }
    crash("unexpected expr: %s\n", types[expr->type]);
  }
};

int main() {
  for (;;) {
    printf("> ");
    std::string input;
    std::getline(std::cin, input);
    if (input.empty()) continue;
    if (input == "exit") break;
    AST ast = AST::parse(input);
    if (!ast.errors.empty()) {
      for (auto& error: ast.errors)
        fprintf(stderr, "%s\n", error.c_str());
      continue;
    }
    Evaluator evaluator{std::move(ast.root)};
    const UnitNumber result = evaluator.evaluate();
    fprintf(stdout, "%lu\n", result);
  }
  return EXIT_SUCCESS;
}
