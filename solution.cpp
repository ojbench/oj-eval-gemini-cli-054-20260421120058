#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>

using namespace std;

struct Term {
    long long a;
    int b, c, d;

    bool operator<(const Term& other) const {
        if (b != other.b) return b > other.b;
        if (c != other.c) return c > other.c;
        return d > other.d;
    }

    bool is_same_type(const Term& other) const {
        return b == other.b && c == other.c && d == other.d;
    }
};

struct Poly {
    vector<Term> terms;

    void simplify() {
        if (terms.empty()) return;
        sort(terms.begin(), terms.end());
        vector<Term> next_terms;
        for (const auto& t : terms) {
            if (t.a == 0) continue;
            if (!next_terms.empty() && next_terms.back().is_same_type(t)) {
                next_terms.back().a += t.a;
                if (next_terms.back().a == 0) next_terms.pop_back();
            } else {
                next_terms.push_back(t);
            }
        }
        terms = next_terms;
    }

    Poly operator+(const Poly& other) const {
        Poly res = *this;
        res.terms.insert(res.terms.end(), other.terms.begin(), other.terms.end());
        res.simplify();
        return res;
    }

    Poly operator-(const Poly& other) const {
        Poly res = *this;
        for (const auto& t : other.terms) {
            res.terms.push_back({-t.a, t.b, t.c, t.d});
        }
        res.simplify();
        return res;
    }

    Poly operator*(const Poly& other) const {
        Poly res;
        for (const auto& t1 : terms) {
            for (const auto& t2 : other.terms) {
                res.terms.push_back({t1.a * t2.a, t1.b + t2.b, t1.c + t2.c, t1.d + t2.d});
            }
        }
        res.simplify();
        return res;
    }

    Poly derivate() const {
        Poly res;
        for (const auto& t : terms) {
            if (t.b > 0) res.terms.push_back({t.a * t.b, t.b - 1, t.c, t.d});
            if (t.c > 0) res.terms.push_back({t.a * t.c, t.b, t.c - 1, t.d + 1});
            if (t.d > 0) res.terms.push_back({-t.a * t.d, t.b, t.c + 1, t.d - 1});
        }
        res.simplify();
        return res;
    }

    string to_string() const {
        if (terms.empty()) return "0";
        string res = "";
        for (size_t i = 0; i < terms.size(); ++i) {
            const auto& t = terms[i];
            long long abs_a = abs(t.a);
            if (t.a > 0) {
                if (i > 0) res += "+";
            } else {
                res += "-";
            }

            bool is_constant = (t.b == 0 && t.c == 0 && t.d == 0);
            if (abs_a != 1 || is_constant) {
                res += std::to_string(abs_a);
            }

            if (t.b > 0) {
                res += "x";
                if (t.b > 1) res += "^" + std::to_string(t.b);
            }
            if (t.c > 0) {
                res += "sin";
                if (t.c > 1) res += "^" + std::to_string(t.c);
                res += "x";
            }
            if (t.d > 0) {
                res += "cos";
                if (t.d > 1) res += "^" + std::to_string(t.d);
                res += "x";
            }
        }
        return res;
    }
};

struct Frac {
    Poly p, q;

    Frac() {
        q.terms.push_back({1, 0, 0, 0});
    }
    Frac(long long val) {
        if (val != 0) p.terms.push_back({val, 0, 0, 0});
        q.terms.push_back({1, 0, 0, 0});
    }
    Frac(Poly _p, Poly _q) : p(_p), q(_q) {}

    Frac operator+(const Frac& other) const {
        return Frac(p * other.q + other.p * q, q * other.q);
    }
    Frac operator-(const Frac& other) const {
        return Frac(p * other.q - other.p * q, q * other.q);
    }
    Frac operator*(const Frac& other) const {
        return Frac(p * other.p, q * other.q);
    }
    Frac operator/(const Frac& other) const {
        return Frac(p * other.q, q * other.p);
    }

    Frac derivate() const {
        return Frac(p.derivate() * q - q.derivate() * p, q * q);
    }

    string to_string() const {
        Poly pp = p;
        Poly qq = q;
        pp.simplify();
        qq.simplify();
        if (pp.terms.empty()) return "0";
        string sp = pp.to_string();
        string sq = qq.to_string();
        if (sq == "1") return sp;
        
        if (pp.terms.size() > 1) sp = "(" + sp + ")";
        if (qq.terms.size() > 1) sq = "(" + sq + ")";
        return sp + "/" + sq;
    }
};

string input;
size_t pos = 0;

char peek() {
    while (pos < input.size() && isspace(input[pos])) pos++;
    if (pos < input.size()) return input[pos];
    return 0;
}

char consume() {
    peek();
    return input[pos++];
}

long long parseNumber() {
    string s = "";
    while (isdigit(peek())) s += consume();
    if (s == "") return 1;
    return stoll(s);
}

Frac parseExpression();

Frac parseAtom() {
    Term t = {1, 0, 0, 0};
    bool has_coeff = false;
    if (isdigit(peek())) {
        t.a = parseNumber();
        has_coeff = true;
    }
    
    while (true) {
        char c = peek();
        if (c == 'x') {
            consume();
            if (peek() == '^') {
                consume();
                t.b += (int)parseNumber();
            } else {
                t.b += 1;
            }
        } else if (c == 's') { // sin
            consume(); // s
            consume(); // i
            consume(); // n
            if (peek() == '^') {
                consume();
                t.c += (int)parseNumber();
                consume(); // x
            } else {
                t.c += 1;
                consume(); // x
            }
        } else if (c == 'c') { // cos
            consume(); // c
            consume(); // o
            consume(); // s
            if (peek() == '^') {
                consume();
                t.d += (int)parseNumber();
                consume(); // x
            } else {
                t.d += 1;
                consume(); // x
            }
        } else {
            break;
        }
    }
    Poly p;
    p.terms.push_back(t);
    p.simplify();
    Poly q;
    q.terms.push_back({1, 0, 0, 0});
    return Frac(p, q);
}

Frac parseFactor() {
    if (peek() == '-') {
        consume();
        return Frac(0) - parseFactor();
    }
    if (peek() == '(') {
        consume();
        Frac res = parseExpression();
        if (peek() == ')') consume();
        return res;
    }
    return parseAtom();
}

Frac parseTerm() {
    Frac res = parseFactor();
    while (peek() == '*' || peek() == '/') {
        char op = consume();
        Frac next = parseFactor();
        if (op == '*') res = res * next;
        else res = res / next;
    }
    return res;
}

Frac parseExpression() {
    Frac res = parseTerm();
    while (peek() == '+' || peek() == '-') {
        char op = consume();
        Frac next = parseTerm();
        if (op == '+') res = res + next;
        else res = res - next;
    }
    return res;
}

int main() {
    if (!getline(cin, input)) return 0;
    pos = 0;
    Frac f = parseExpression();
    cout << f.to_string() << endl;
    cout << f.derivate().to_string() << endl;
    return 0;
}
