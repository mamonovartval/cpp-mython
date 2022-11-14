#include "lexer.h"

#include <algorithm>
#include <charconv>
#include <unordered_map>

using namespace std;

namespace parse {

    bool operator==(const Token& lhs, const Token& rhs) {
        using namespace token_type;

        if (lhs.index() != rhs.index()) {
            return false;
        }
        if (lhs.Is<Char>()) {
            return lhs.As<Char>().value == rhs.As<Char>().value;
        }
        if (lhs.Is<Number>()) {
            return lhs.As<Number>().value == rhs.As<Number>().value;
        }
        if (lhs.Is<String>()) {
            return lhs.As<String>().value == rhs.As<String>().value;
        }
        if (lhs.Is<Id>()) {
            return lhs.As<Id>().value == rhs.As<Id>().value;
        }
        return true;
    }

    bool operator!=(const Token& lhs, const Token& rhs) {
        return !(lhs == rhs);
    }

    std::ostream& operator<<(std::ostream& os, const Token& rhs) {
        using namespace token_type;

#define VALUED_OUTPUT(type) \
    if (auto p = rhs.TryAs<type>()) return os << #type << '{' << p->value << '}';

        VALUED_OUTPUT(Number);
        VALUED_OUTPUT(Id);
        VALUED_OUTPUT(String);
        VALUED_OUTPUT(Char);

#undef VALUED_OUTPUT

#define UNVALUED_OUTPUT(type) \
    if (rhs.Is<type>()) return os << #type;

        UNVALUED_OUTPUT(Class);
        UNVALUED_OUTPUT(Return);
        UNVALUED_OUTPUT(If);
        UNVALUED_OUTPUT(Else);
        UNVALUED_OUTPUT(Def);
        UNVALUED_OUTPUT(Newline);
        UNVALUED_OUTPUT(Print);
        UNVALUED_OUTPUT(Indent);
        UNVALUED_OUTPUT(Dedent);
        UNVALUED_OUTPUT(And);
        UNVALUED_OUTPUT(Or);
        UNVALUED_OUTPUT(Not);
        UNVALUED_OUTPUT(Eq);
        UNVALUED_OUTPUT(NotEq);
        UNVALUED_OUTPUT(LessOrEq);
        UNVALUED_OUTPUT(GreaterOrEq);
        UNVALUED_OUTPUT(None);
        UNVALUED_OUTPUT(True);
        UNVALUED_OUTPUT(False);
        UNVALUED_OUTPUT(Eof);

#undef UNVALUED_OUTPUT

        return os << "Unknown token :("sv;
    }

    Lexer::Lexer(std::istream& in) {
        using namespace parse;
        using namespace token_type;

        std::string inp_line;
        while (getline(in, inp_line)) {

            if (IsEmptyLine(inp_line)) {
                continue;
            }

            SetIndent(TrimLine(inp_line));
            std::istringstream istring(inp_line);
            ReadLine(istring);
        }

        SetIndent(0);

        tokens_.push_back(Eof{});
    }

    const Token& Lexer::CurrentToken() const {
        if (index_ < tokens_.size()) {
            return tokens_[index_];
        }
        throw std::logic_error("Not implemented"s);
    }

    Token Lexer::NextToken() {
        if ((index_ + 1) < tokens_.size()) {
            index_++;
        }
        return CurrentToken();
    }
    
    size_t Lexer::TrimLine(std::string& in) const {
        size_t result = in.find_first_not_of(' ', 0);
        if (result >= 2) {
            result /= 2;
        }
        in = in.substr(result);
        return result;
    }

    void Lexer::SetIndent(const size_t new_level) {
        using namespace parse::token_type;
        for (size_t i = number_spaces; i < new_level; ++i) {
            tokens_.push_back(Indent({}));
        }
        for (size_t i = number_spaces; i > new_level; --i) {
            tokens_.push_back(Dedent({}));
        }
        if (number_spaces != new_level) {
            number_spaces = new_level;
        }
    }

    void Lexer::ReadLine(std::istringstream& istring) {
        using namespace parse::token_type;
        char ch;
        bool isNewLine = false;
        while (istring.get(ch)) {
            if (ch == ' ') {
                continue;
            }
            if (ch == '#') {
                istring.ignore(numeric_limits<streamsize>::max(), '\n');
                tokens_.push_back(Newline{});
                return;
            }

            isNewLine = true;
            if (isdigit(ch)) {
                ReadNumber(istring);
            }
            else if (isprint(ch) || isspace(ch)) {
               ReadSign(istring, ch);
            }
        }
        if (isNewLine) {
            tokens_.push_back(Newline{});
        }
    }

    void Lexer::ReadId(std::istringstream& istring) {
        using namespace parse::token_type;
        std::string s;
        char c;
        while (istring.get(c)) {
            if ((isspace(c) || c == ' ') || (ispunct(c) && c != '_')) {
                if (ispunct(c) && c != '_') {
                    istring.unget();
                }
                break;
            }
            s += c;
        }
        if (s == "class") {
            tokens_.push_back(Class({}));
        }
        else if (s == "return") {
            tokens_.push_back(Return({}));
        }
        else if (s == "if") {
            tokens_.push_back(If({}));
        }
        else if (s == "else") {
            tokens_.push_back(Else({}));
        }
        else if (s == "def") {
            tokens_.push_back(Def({}));
        }
        else if (s == "print") {
            tokens_.push_back(Print({}));
        }
        else if (s == "or") {
            tokens_.push_back(Or({}));
        }
        else if (s == "None") {
            tokens_.push_back(None({}));
        }
        else if (s == "and") {
            tokens_.push_back(And({}));
        }
        else if (s == "not") {
            tokens_.push_back(Not({}));
        }
        else if (s == "True") {
            tokens_.push_back(True({}));
        }
        else if (s == "False") {
            tokens_.push_back(False({}));
        }
        else {
            tokens_.push_back(Id({ s }));
        }
    }

    void Lexer::ReadSign(std::istringstream& istring, char ch)
    {
        using namespace parse::token_type;
        switch (ch) {
        case '=': case '!': case '<': case'>': {
            if (istring.peek() == '=') {
                if (ch == '=') {
                    tokens_.push_back(Eq({}));
                }
                else if (ch == '!') {
                    tokens_.push_back(NotEq({}));
                }
                else if (ch == '<') {
                    tokens_.push_back(LessOrEq({}));
                }
                else {
                    tokens_.push_back(GreaterOrEq({}));
                }
                istring.get();
                break;
            }
            [[fallthrough]];
        }

        case '*': case '/': case '+': case '-': case '(': case ')': case '?':
        case ',': case '.': case ':': case ';': case '\t': case '\n': {

            tokens_.push_back(Char{ ch });
            break;
        }
        case '\'': case '\"': {
            ReadString(istring, ch);
            break;
        }
        default: {
            istring.unget();
            ReadId(istring);
        }
        }
    }

    void Lexer::ReadString(std::istringstream& in, const char d) {
        std::string str;
        char ch;
        while (in.get(ch)) {
            if (ch == d) {
                break;
            }
            if (ch == '\\') {
                in.get(ch);
                switch (ch) {
                case 'n':
                    ch = '\n';
                    break;
                case 't':
                    ch = '\t';
                }
                str += ch;
            }
            else {
                str += ch;
            }

        }
        tokens_.push_back(parse::token_type::String{ str });
    }

    void Lexer::ReadNumber(std::istringstream& in) {
        in.unget();
        int d;
        in >> d;
        tokens_.push_back(parse::token_type::Number{ d });
    }

    bool Lexer::IsEmptyLine(const std::string_view line) const {
        if (line.size() == 0) {
            return true;
        }
        size_t p = line.find_first_not_of(' ');
        if (line[p] == '#' || (p > 0 && p == (line.size() - 1))) {
            return true;
        }
        return false;
    }

    int Lexer::CountSpace(const std::string_view line)const {
        size_t result = line.find_first_not_of(' ', 0);
        if (result >= 2) {
            result /= 2;
        }
        return result;
    }
    
}  // namespace parse