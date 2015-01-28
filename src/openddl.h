#ifndef __OPENDDL_H_INCLUDED
#define __OPENDDL_H_INCLUDED

////////////////////////////////////////////////////////////////////////////////
//
// (C) Andy Thomason 2015 (MIT license)
//
// OpenDDL parser
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation the 
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
// AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

#include <exception>
#include <bitset>
#include <cstdint>

struct openddl_exception : std::exception {
  const char *location;

  openddl_exception(const char *message, const char *location) :
    std::exception(message), location(location)
  {
  }
};

struct def_openddl_traits {
  static void do_throw(const char *message, const char *location) {
    throw(openddl_exception(message, location));
  }
};

template <class traits=def_openddl_traits> class openddl {
  enum : uint8_t {
    dt_bool,
    dt_int8,
    dt_int16,
    dt_int32,
    dt_int64,
    dt_unsigned_int8,
    dt_unsigned_int16,
    dt_unsigned_int32,
    dt_unsigned_int64,
    dt_float,
    dt_double,
    dt_string,
    dt_ref,
    dt_type,
  };

  const char *src;
  const char *end;

  #pragma warning(disable : 4996)
  template<class... args> bool err(args... a) {
    static char buf[256];
    sprintf(buf, a...);
    traits::do_throw(buf, src);
    return false;
  }

  bool err_unexpected_eof() {
    return err("unexpected EOF");
  }

  bool skip_ws() {
    while (src != end && (*src & 0xff) <= ' ') {
      ++src;
    }
    while (src + 2 <= end && src[0] == '/' && src[1] == '/') {
      src += 2;
      while (src != end && *src != '\n') {
        ++src;
      }
      while (src != end && (*src & 0xff) <= ' ') {
        ++src;
      }
    }
    return true;
  }

  bool is(char symbol) {
    if (src != end && *src == symbol) {
      ++src;
      skip_ws();
      return true;
    }
    return false;
  }

  bool is_chr(char symbol) {
    if (src != end && *src == symbol) {
      ++src;
      return true;
    }
    return false;
  }

  bool is_chr(char first, char last) {
    if (src != end && *src >= first && *src <= last) {
      ++src;
      return true;
    }
    return false;
  }

  bool is_chr(const std::bitset<128> &set) {
    if (src != end && (*src & 0x80) == 0 && set[*src]) {
      ++src;
      return true;
    }
    return false;
  }

  bool expect(char symbol) {
    if (src == end) return err_unexpected_eof();
    if (is(symbol)) {
      return true;
    }
    return err("expected %c", symbol);
  }

  std::string identifier_val;
  std::string string_val;
  uint64_t literal_val;


  //  Listing A.1. This is the formal grammar defining the OpenDDL syntax.

  std::bitset<128> id_start_set;
  std::bitset<128> id_mid_set;

  //  identifier ::= [A-Za-z_] [0-9A-Za-z_]*
  bool identifier() {
    const char *save = src;
    if (is_chr(id_start_set)) {
      while (is_chr(id_mid_set)) ;
      identifier_val.assign(save, src);
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  name ::= ("$" | "%") identifier
  bool name() {
    const char *save = src;
    if ( (is_chr('$') || is_chr('%')) && identifier()) {
      return true;
    }
    src = save;
    return false;
  }

  //  reference ::= name ("%" identifier)* | "null"
  bool reference() {
    const char *save = src;
    if (identifier() && identifier_val == "null") {
      return true;
    } else if (name()) {
      while (is_chr('%') && identifier()) {
      }
      return true;
    }
    src = save;
    return false;
  }

  std::bitset<128> hex_digit_set;

  //  hex-digit ::= [0-9A-Fa-f]
  bool hex_digit() {
    return is_chr(hex_digit_set);
  }

  //  escape-char ::= '\"' | "\'" | "\?" | "\\" | "\a" | "\b" | "\f" | "\n" | "\r" | "\t" | "\v" | "\x" hex-digit hex-digit

  std::bitset<128> escape_char_set;

  uint64_t to_hex(char c) {
    return (c <= '9' ? c : (c-6)) & 0x0f;
  }

  bool escape_char() {
    const char *save = src;
    if (is_chr('\\') && is_chr(escape_char_set)) {
      if (is_chr('x') && hex_digit() && hex_digit()) {
        literal_val = to_hex(src[-2]) * 16 + to_hex(src[-1]);
        return true;
      } else {
        char c = src[-1];
        literal_val =
          c == 'a' ? '\a' : 
          c == 'b' ? '\b' : 
          c == 'f' ? '\f' : 
          c == 'n' ? '\n' : 
          c == 'r' ? '\r' : 
          c == 't' ? '\t' : 
          c == 'v' ? '\v' : 
          c
        ;
        ++src;
        return true;
      }
    }
    src = save;
    return false;
  }

  //  bool-literal ::= "false" | "true"

  bool bool_literal() {
    const char *save = src;
    if (src != end && (*src == 'f' || *src == 't') && identifier()) {
      if (identifier_val == "true") {
        literal_val = 1;
        return true;
      } else if (identifier_val == "false") {
        literal_val = 0;
        return true;
      }
    }
    src = save;
    return false;
  }

  //  decimal-literal ::= [0-9]+
  bool decimal_literal() {
    const char *save = src;
    if (is_chr('0', '9')) {
      uint64_t val = src[-1] - '0';
      while (is_chr('0', '9')) {
        val = val * 10 + (src[-1] - '0');
      }
      literal_val = val;
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  hex-literal ::= ("0x" | "0X") hex-digit+
  bool hex_literal() {
    const char *save = src;
    if (is_chr('0') && (is_chr('x') || is_chr('X'))) {
      if (hex_digit()) {
        uint64_t val = to_hex(src[-1]);
        while (hex_digit()) {
          val = val * 16 + to_hex(src[-1]);
        }
        literal_val = val;
        skip_ws();
        return true;
      }
    }
    src = save;
    return false;
  }

  //  binary-literal ::= ("0b" | "0B") ("0" | "1")+
  bool binary_literal() {
    const char *save = src;
    if (is_chr('0') && (is_chr('b') || is_chr('B'))) {
      if (is_chr('0', '1')) {
        uint64_t val = src[-1] & 1;
        while (is_chr('0', '1')) {
          val = val * 2 + (src[-1] & 1);
        }
        literal_val = val;
        skip_ws();
        return true;
      }
    }
    src = save;
    return false;
  }

  //  char-literal ::= "'" ([#x20-#x26#x28-#x5B#x5D-#x7E] | escape-char)+ "'"
  std::bitset<128> char_literal_set;

  bool char_literal() {
    const char *save = src;
    if (is_chr('\'')) {
      uint64_t val = 0;
      while (src != end && *src != '\'') {
        if (escape_char()) {
          val = val * 256 + literal_val;
        } else {
          val = val * 256 + (*src++ & 0xff);
        }
      }
      if (src == end) return err_unexpected_eof();
      ++src;
      literal_val = val;
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  integer-literal ::= ("+" | "-")? (decimal-literal | hex-literal | binary-literal
  //    | char-literal)
  bool integer_literal() {
    const char *save = src;
    int sign = 0;
    if (is_chr('+') || is_chr('-')) {
      sign = src[-1] == '-';
    }
    if (hex_literal() || decimal_literal() || binary_literal() || char_literal()) {
      literal_val = sign ? -(int64_t)literal_val : literal_val;
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  float-literal ::= ("+" | "-")? (([0-9]+ ("." [0-9]*)? | "." [0-9]+)
  //    (("e" | "E") ("+" | "-")? [0-9]+)?
  //    | hex-literal | binary-literal)
  bool float_literal() {
    const char *save = src;
    int sign = 0;
    if (is_chr('+') || is_chr('-')) {
      sign = src[-1] == '-';
    }
    if (hex_literal() || binary_literal()) {
      literal_val = (uint32_t)(sign ? literal_val ^ 0x80000000 : literal_val);
      return true;
    } else if (is_chr('0', '9')) {
      double val = src[-1] - '0';
      while (is_chr('0', '9')) {
        val = val * 10 + src[-1] - '0';
      }
      if (is_chr('.')) {
        double denom = 1;
        while (is_chr('0', '9')) {
          denom *= 10;
          val = val * 10 + src[-1] - '0';
        }
        val /= denom;
      }
      if ((is_chr('e') || is_chr('E')) && is_chr('0', '9')) {
        double exp = src[-1] - '0';
        while (is_chr('0', '9')) {
          exp = exp * 10 + src[-1] - '0';
        }
        int sign = 0;
        if (is_chr('+') || is_chr('-')) {
          sign = src[-1] == '-';
        }
        val *= pow(10, sign ? -exp : exp);
      }
      union {
        uint32_t u32;
        float f32;
      } u;
      u.f32 = (float)val;
      literal_val = u.u32;
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  string-literal ::= ('"' ([#x20-#x21#x23-#x5B#x5D-#x7E#xA0-#xD7FF#xE000-#xFFFD#x010000-#x10FFFF]
  //    | escape-char | "\u" hex-digit hex-digit hex-digit hex-digit
  //    | "\U" hex-digit hex-digit hex-digit hex-digit hex-digit hex-digit)* '"')+
  bool string_literal() {
    const char *save = src;
    if (is_chr('"')) {
      string_val.resize(0);
      while (src != end && *src != '"') {
        uint64_t val = 0;
        if (*src == '\\' && src+5 < end && src[1] == 'u') {
          val = ((to_hex(src[2]) * 16 + to_hex(src[3])) * 16 + to_hex(src[4])) * 16 + to_hex(src[5]);
        } else if (*src == '\\' && src+7 < end && src[1] == 'U') {
          val = ((((to_hex(src[2]) * 16 + to_hex(src[3])) * 16 + to_hex(src[4])) * 16 + to_hex(src[5])) * 16 + to_hex(src[6])) * 16 + to_hex(src[7]);
        } else if (escape_char()) {
          val = literal_val;
        } else {
          val = (*src++ & 0xff);
        }
        // todo: support UTF8
        string_val.push_back((uint8_t)val);
      }
      if (src == end) return err_unexpected_eof();
      ++src;
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  data-type ::= "bool" | "int8" | "int16" | "int32" | "int64" | "unsigned_int8"
  //    | "unsigned_int16" | "unsigned_int32" | "unsigned_int64"
  //    | "float" | "double" | "string" | "ref" | "type"

  bool data_type() {
    const char *save = src;
    if (identifier()) {
      switch (identifier_val[0]) {
        case 'b': {
          if (identifier_val == "bool") { literal_val = dt_bool; return true; }
        } break;
        case 'i': {
          if (identifier_val == "int8") { literal_val = dt_int8; return true; }
          if (identifier_val == "int16") { literal_val = dt_int16; return true; }
          if (identifier_val == "int32") { literal_val = dt_int32; return true; }
          if (identifier_val == "int64") { literal_val = dt_int64; return true; }
        } break;
        case 'u': {
          if (identifier_val == "unsigned_int8") { literal_val = dt_unsigned_int8; return true; }
          if (identifier_val == "unsigned_int16") { literal_val = dt_unsigned_int16; return true; }
          if (identifier_val == "unsigned_int32") { literal_val = dt_unsigned_int32; return true; }
          if (identifier_val == "unsigned_int64") { literal_val = dt_unsigned_int64; return true; }
        } break;
        default: {
          if (identifier_val == "float") { literal_val = dt_float; return true; }
          if (identifier_val == "double") { literal_val = dt_double; return true; }
          if (identifier_val == "string") { literal_val = dt_string; return true; }
          if (identifier_val == "ref") { literal_val = dt_ref; return true; }
          if (identifier_val == "type") { literal_val = dt_type; return true; }
        }
      }
    }
    src = save;
    return false;
  }

  //  data-list ::= bool-literal ("," bool-literal)*
  //    | integer-literal ("," integer-literal)*
  //    | float-literal ("," float-literal)*
  //    | string-literal ("," string-literal)*
  //    | reference ("," reference)*
  //    | data-type ("," data-type)*

  template <class _Fn> bool data_list(_Fn literal) {
    while (is(',') && literal()) {
    }
    return true;
  }

  bool data_list() {
    const char *save = src;
    if (bool_literal()) {
      return data_list([=](){ return bool_literal(); });
    } else if (float_literal()) {
      return data_list([=](){ return float_literal(); });
    } else if (integer_literal()) {
      return data_list([=](){ return integer_literal(); });
    } else if (string_literal()) {
      return data_list([=](){ return string_literal(); });
    } else if (reference()) {
      return data_list([=](){ return reference(); });
    } else if (data_type()) {
      return data_list([=](){ return data_type(); });
    }
    src = save;
    return false;
  }

  //  data-array-list ::= "{" bool-literal ("," bool-literal)* "}" ("," "{" bool-literal
  //  ("," bool-literal)* "}")*
  //    | "{" integer-literal ("," integer-literal)* "}" ("," "{" integer-literal
  //  ("," integer-literal)* "}")*
  //    | "{" float-literal ("," float-literal)* "}" ("," "{" float-literal
  //  ("," float-literal)* "}")*
  //    | "{" string-literal ("," string-literal)* "}" ("," "{" string-literal
  //  ("," string-literal)* "}")*
  //    | "{" reference ("," reference)* "}" ("," "{" reference
  //  ("," reference)* "}")*
  //    | "{" data-type ("," data-type)* "}" ("," "{" data-type
  //  ("," data-type)* "}")*

  template <class _Fn> bool data_array_list(_Fn literal) {
    while (is(',') && literal()) {
    }
    if (!expect('}')) return false;

    while (is(',') && is('{') && literal()) {
      while (is(',') && literal()) {
      }
      if (!expect('}')) return false;
    }

    return true;
  }

  bool data_array_list() {
    const char *save = src;
    if (is('{')) {
      if (bool_literal()) {
        return data_array_list([=](){ return bool_literal(); });
      } else if (float_literal()) {
        return data_array_list([=](){ return float_literal(); });
      } else if (integer_literal()) {
        return data_array_list([=](){ return integer_literal(); });
      } else if (string_literal()) {
        return data_array_list([=](){ return string_literal(); });
      } else if (reference()) {
        return data_array_list([=](){ return reference(); });
      } else if (data_type()) {
        return data_array_list([=](){ return data_type(); });
      }
    }
    src = save;
    return false;
  }

  //  property ::= identifier "=" (bool-literal | integer-literal | float-literal
  //    | string-literal | reference | data-type)

  bool property() {
    const char *save = src;
    if (identifier() && is('=')) {
      if (bool_literal()) {
        return true;
      } else if (float_literal()) {
        return true;
      } else if (integer_literal()) {
        return true;
      } else if (string_literal()) {
        return true;
      } else if (reference()) {
        return true;
      } else if (data_type()) {
        return true;
      }
    }
    src = save;
    return false;
  }
  
  //  structure ::=
  //    data-type (name? "{" data-list? "}" | "[" integer-literal "]" name? "{" data-array-list? "}")
  //    | identifier name? ("(" (property ("," property)*)? ")")? "{" structure* "}"

  bool structure() {
    const char *save = src;
    if (data_type()) {
      if (is('[')) {
        literal_val = 0;
        if (!integer_literal()) {
          return err("expected integer literal");
        }
        if (!expect(']')) return false;

        if (name()) {
        }

        if (!expect('{')) return false;
        if (data_array_list()) {
        }
        if (!expect('}')) return false;
        return true;
      } else {
        if (name()) {
        }

        if (!expect('{')) return false;
        if (data_list()) {
        }
        if (!expect('}')) return false;
        return true;
      }
    } else if (identifier()) {
      if (name()) {
      }
      if (is('(')) {
        while (property()) {
          if (!is(',')) break;
        }
        if (!expect(')')) return false;
      }
      if (!expect('{')) return false;
      while (structure()) {
      }
      if (!expect('}')) return false;
      return true;
    }
    src = save;
    return false;
  }

  //  file ::= structure*

  bool file() {
    if (src > end) return false;

    while (structure()) {
    }

    return src == end;
  }

public:
  openddl(const char *begin, const char *end) {
    hex_digit_set.reset();
    id_start_set.reset();
    id_start_set.set('_');
    for (int i = 0; i != 26; ++i) { id_start_set.set('A' + i); id_start_set.set('a' + i); }
    for (int i = 0; i != 6; ++i) { hex_digit_set.set('A' + i); hex_digit_set.set('a' + i); }

    escape_char_set.reset();
    for (auto v : "\"'?\\abfnrtvx") {
      if (v) escape_char_set.set(v);
    }

    id_mid_set = id_start_set;
    for (int i = 0; i != 10; ++i) { id_mid_set.set('0' + i); hex_digit_set.set('0' + i); }

    src = begin;
    this->end = end;
    skip_ws();
    file();
  }
};


#endif

