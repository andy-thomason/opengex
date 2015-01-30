#ifndef __parser_H_INCLUDED
#define __parser_H_INCLUDED

////////////////////////////////////////////////////////////////////////////////
//
// (C) Andy Thomason 2015 (MIT license)
//
// openddl parser
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
#include <vector>
#include <map>
#include <ostream>
#include <iostream>

namespace openddl {

typedef std::uint8_t uint8_t;
typedef std::uint32_t uint32_t;
typedef std::uint64_t uint64_t;

typedef uint32_t node_index;
typedef uint32_t data_index;

enum class data_type_t : uint8_t {
  dt_invalid,
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
  dt_max,
};

size_t data_type_size(data_type_t type) {
  switch (type) {
    case data_type_t::dt_type:
    case data_type_t::dt_bool:
    case data_type_t::dt_int8:
    case data_type_t::dt_unsigned_int8: return 1;

    case data_type_t::dt_int16:
    case data_type_t::dt_unsigned_int16: return 2;

    default:
    case data_type_t::dt_int32:
    case data_type_t::dt_unsigned_int32:
    case data_type_t::dt_float: return 4;

    case data_type_t::dt_int64:
    case data_type_t::dt_unsigned_int64:
    case data_type_t::dt_double: return 8;

    case data_type_t::dt_string: return sizeof(node_index);
    case data_type_t::dt_ref: return sizeof(data_index);
  }
}

const char *data_type_name(data_type_t type) {
  static const char *names[] = {
    "invalid",
    "bool",
    "int8",
    "int16",
    "int32",
    "int64",
    "unsigned_int8",
    "unsigned_int16",
    "unsigned_int32",
    "unsigned_int64",
    "float",
    "double",
    "string",
    "ref",
    "type",
  };
  return type < data_type_t::dt_max ? names[(int)type] : names[0];
}

class file {
public:
  /// construct an empty openddl file.
  file() {
    // node_index 0 is "null"
    nodes.emplace_back();

    // data_index 0 is ""
    data.push_back(0);
  }

  /// find a global node or return 0.
  node_index get_global(const std::string &name) const {
    for (node_index ni = 0; ni != nodes.size(); ++ni) {
      if (name == get_name(ni)) {
        return ni;
      }
    }
    return 0;
  }

  /// find a child node or return 0.
  node_index get_child(node_index parent, const std::string &name) {
    if (!name.empty()) {
      for (node_index child = nodes[parent].children; child != 0; child = nodes[child].siblings) {
        if (name == get_name(child)) {
          return child;
        }
      }
    }
    return 0;
  }

  /// add a new child to a node. return the node index.
  node_index add_child(node_index parent) {
    node_index result = (node_index)nodes.size();
    nodes.emplace_back();
    node &par = nodes[parent];
    node &child = nodes[result];

    if (par.last_child) {;
      nodes[par.last_child].siblings = result;
    } else {
      par.children = result;
    }
    par.last_child = result;
    return result;
  }

  /// set the ind for a node. (eg. GeometryNode)
  void set_kind(node_index ni, const std::string &val) {
    std::cout << "node kind " << ni << " " << val << "\n";
    nodes[ni].kind = add_string(val);
  }

  /// set the name for a node. (eg. $object1)
  void set_name(node_index ni, const std::string &val) {
    std::cout << "node name " << ni << " " << val << "\n";
    nodes[ni].name = add_string(val);
  }

  /// set the data for a node.
  void set_data(node_index ni, const std::vector<uint8_t> &val) {
    std::cout << "node data " << ni << " " << val.size() << "\n";
    nodes[ni].data = add_data(val);
    nodes[ni].data_size = (uint32_t)val.size();
  }

  /// set the data type for a node. (eg. dt_float)
  void set_data_type(node_index ni, data_type_t type) {
    nodes[ni].data_type = type;
  }

  /// set the group size for a node (eg. float[3])
  void set_data_group_size(node_index ni, uint8_t size) {
    nodes[ni].data_group_size = size;
  }

  /// set the properties for a node
  void set_props(node_index ni, const std::vector<uint8_t> &val) {
    std::cout << "node props " << ni << " " << val.size() << "\n";
    nodes[ni].props = add_data(val);
  }

  /// dump the node for debugging.
  void dump_node(std::ostream &os, node_index parent, std::string &indent, std::string &temp) {
    node &par = nodes[parent];
    for (node_index child = par.children; child != 0; child = nodes[child].siblings) {
      node &ch = nodes[child];
      const char *kind = get_kind(child);
      if (!kind[0]) {
        const uint8_t *data = get_data(child);
        size_t group_size = get_data_group_size(child);
        size_t size = get_data_size(child);
        data_type_t type = get_data_type(child);
        const uint8_t *data_end = data + size;
        size_t dt_size = data_type_size(type);
        if (group_size) {
          os << indent << data_type_name(type) << "[" << std::dec << group_size << "] { ";

          temp.resize(0);
          while (data < data_end) {
            temp.append("{");
            for (size_t i = 0; i != group_size; ++i ) {
              if (data < data_end) get_data_string(temp, type, dt_size, data);
              data += dt_size;
              if (i + 1 < group_size) temp.append(", ");
            }
            temp.append("}");
            if (data < data_end) temp.append(", ");
          }
          os << temp << " }\n";
        } else {
          os << indent << data_type_name(get_data_type(child)) << " { ";
          temp.resize(0);
          while (data < data_end) {
            if (data < data_end) get_data_string(temp, type, dt_size, data);
            data += dt_size;
            if (data < data_end) temp.append(", ");
          }
          os << temp << " }\n";
        }
      } else {
        const uint8_t *props = get_properites(child);
        if (props[0]) {
          os << indent << kind << " " << get_prop_string(child, temp) << " { " << get_name(child) << "\n";
        } else {
          os << indent << kind << " { " << get_name(child) << "\n";
        }
        indent.append("  ");
        dump_node(os, child, indent, temp);
        indent.resize(indent.size() - 2);
        os << indent << "}\n";
      }
    }
  }

  /// get the name of a node (eg. $object)
  const char *get_name(node_index child) const {
    return (const char*)(data.data() + nodes[child].data);
  }

  /// get the kind of a node (eg. GeometryNode)
  const char *get_kind(node_index child) const {
    return (const char*)(data.data() + nodes[child].kind);
  }

  /// get an abitrary string, such as from a property
  const char *get_string(data_index index) const {
    return (const char*)(data.data() + index);
  }

  std::pair<data_type_t, uint64_t> get_property(node_index child, const std::string &name) const {
    for (const uint8_t *p = data.data() + nodes[child].props; *p; ) {
      const char *this_name = (const char*)p;
      while (*p) ++p;
      p += 2;
      data_type_t type = (data_type_t)p[-1];
      size_t size = data_type_size(type);
      if (name == this_name) {
        uint64_t value = 0;
        for (size_t i = 0; i != size; ++i) {
          value |= p[i] << (i*8);
        }
        return std::make_pair(type, (uint64_t)0);
      }
      p += size;
    }
    return std::make_pair(data_type_t::dt_invalid, (uint64_t)0);
  }

  /// get properties as a string
  std::string &get_prop_string(node_index child, std::string &str) const {
    str.resize(0);
    str.append("(");
    for (const uint8_t *p = data.data() + nodes[child].props; *p; ) {
      const char *this_name = (const char*)p;
      while (*p) ++p;
      str.append(this_name, (const char*)p);
      str.append(" = ");
      p += 2;
      data_type_t type = (data_type_t)p[-1];
      size_t size = data_type_size(type);
      get_data_string(str, type, size, p);
      p += size;
      if (*p) str.append(", ");
    }
    str.append(")");
    return str;
  }

  /// Get the size of the data for a node.
  data_index get_data_size(node_index child) const {
    return nodes[child].data_size;
  }

  /// Get the type of the data for a node. (eg. dt_float)
  data_type_t get_data_type(node_index child) const {
    return nodes[child].data_type;
  }

  /// Get the group size of the data for a node. (eg. 3 in float[3])
  uint8_t get_data_group_size(node_index child) const {
    return nodes[child].data_group_size;
  }

  /// Get a pointer to the data for a node
  const uint8_t *get_data(node_index child) const {
    return data.data() + nodes[child].data;
  }

  /// Get a pointer to the properties for a node
  const uint8_t *get_properites(node_index child) const {
    return data.data() + nodes[child].props;
  }

  /// add a string to the file and get the data index.
  data_index add_string(const std::string &str) {
    data_index result = (data_index)data.size();
    data.insert(data.end(), str.begin(), str.end());
    data.push_back(0);
    return result;
  }

  /// add data to the file and get the data index.
  data_index add_data(const std::vector<uint8_t> &val) {
    data_index result = (data_index)data.size();
    data.insert(data.end(), val.begin(), val.end());
    return result;
  }

  /// add a literal to the file and get the data index.
  data_index add_literal(uint64_t literal_val, size_t size) {
    data_index result = (data_index)data.size();
    while (size--) {
      data.push_back((uint8_t)literal_val);
      literal_val >>= 8;
    }
    return result;
  }

private:
  /// internal POD representation of a node.
  struct node {
    data_index kind = 0;
    data_index name = 0;
    data_index data = 0;
    data_index props = 0;
    data_index data_size = 0;

    node_index parent = 0;
    node_index children = 0;
    node_index last_child = 0;
    node_index siblings = 0;

    data_type_t data_type = data_type_t::dt_invalid;
    uint8_t data_group_size = 0;
  };

  /// dump binary data
  std::string &get_data_string(std::string &temp, data_type_t type, size_t size, const uint8_t *data) const {
    const uint8_t *begin = data;
    uint64_t value = 0;
    for (size_t i = 0; i != size; ++i) {
      value |= *data++ << (i*8);
    }
    char tmp[32];

    switch (type) {
      case data_type_t::dt_bool: {
        temp.append(value ? "true" : "false");
      } break;
      default:
      case data_type_t::dt_int8: 
      case data_type_t::dt_int16:
      case data_type_t::dt_int32:
      case data_type_t::dt_int64: {
        _i64toa_s((std::int64_t)value << (8-size) >> (8 - size), tmp, sizeof(tmp), 10);
        temp.append(tmp);
      } break;
      case data_type_t::dt_unsigned_int8:
      case data_type_t::dt_unsigned_int16:
      case data_type_t::dt_unsigned_int32:
      case data_type_t::dt_unsigned_int64: {
        _ui64toa_s((std::int64_t)value << (8-size) >> (8 - size), tmp, sizeof(tmp), 10);
        temp.append(tmp);
      } break;
      case data_type_t::dt_float: {
        tmp[0] = '0'; tmp[1] = 'x';
        _ui64toa_s((std::uint32_t)value, tmp+2, sizeof(tmp)-2, 16);
        temp.append(tmp);
      } break;

      case data_type_t::dt_double: {
        tmp[0] = '0'; tmp[1] = 'x';
        _ui64toa_s(value, tmp+2, sizeof(tmp)-2, 16);
        temp.append(tmp);
      } break;
      case data_type_t::dt_string: {
        // todo: handle escapes
        temp.append("\"");
        temp.append(get_string((data_index)value));
        temp.append("\"");
      } break;
      case data_type_t::dt_ref: {
        temp.append(value ? get_name((node_index)value) : "null");
      } break;
      case data_type_t::dt_type: {
        temp.append(data_type_name((data_type_t)value));
      } break;
    }
    return temp;
  }

  std::vector<node> nodes;
  std::vector<uint8_t> data;
};

/// operator for serialising a file.
std::ostream &operator<<(std::ostream &os, openddl::file &file) {
  std::string indent;
  std::string temp;
  file.dump_node(os, 0, indent, temp);
  return os;
}

/// parser for openddl ASCII file format.
template <class _Char, class _Err> class parser {
  typedef typename _Char char_t;
  const char_t *begin;
  const char_t *src;
  const char_t *end;
  int line_number;
  file *ddl;

  std::string identifier_val;
  std::string string_val;
  uint64_t literal_val;
  std::vector<uint8_t> data_val;
  _Err err_func;

  template <class... args> bool err(args... a) {
    char tmp[256];
    _snprintf_s(tmp, _TRUNCATE, a...);
    return err_func(tmp, line_number);
  }

  bool err_unexpected_eof() {
    return err("unexpected EOF");
  }

  bool skip_ws() {
    while (src != end && (*src & 0xff) <= ' ') {
      if (*src == '\n') line_number++;
      ++src;
    }
    while (src + 2 <= end && src[0] == '/' && src[1] == '/') {
      src += 2;
      while (src != end && *src != '\n') {
        ++src;
      }
      while (src != end && (*src & 0xff) <= ' ') {
        if (*src == '\n') line_number++;
        ++src;
      }
    }
    return true;
  }

  bool is(char_t symbol) {
    if (src != end && *src == symbol) {
      ++src;
      skip_ws();
      return true;
    }
    return false;
  }

  bool is_chr(char_t symbol) {
    if (src != end && *src == symbol) {
      ++src;
      return true;
    }
    return false;
  }

  bool is_chr(char_t first, char_t last) {
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

  bool expect(char_t symbol) {
    if (src == end) {
      return err_unexpected_eof();
    }
    if (is(symbol)) {
      return true;
    }
    return err("expected %c", symbol);
  }

  //  Listing A.1. This is the formal grammar defining the parser syntax.

  std::bitset<128> id_start_set;
  std::bitset<128> id_mid_set;

  //  identifier ::= [A-Za-z_] [0-9A-Za-z_]*
  bool identifier() {
    const char_t *save = src;
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
    const char_t *save = src;
    if ((is_chr('$') || is_chr('%')) && is_chr(id_start_set)) {
      while (is_chr(id_mid_set)) ;
      identifier_val.assign(save, src);
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  reference ::= name ("%" identifier)* | "null"
  bool reference(node_index parent) {
    const char_t *save = src;
    
    if (identifier() && identifier_val == "null") {
      literal_val = 0;
      return true;
    } else if (name()) {
      node_index this_node = identifier_val[0] == '$' ? ddl->get_global(identifier_val) : ddl->get_child(parent, identifier_val);
      while (is_chr('%') && identifier()) {
        this_node = ddl->get_child(this_node, identifier_val);
      }
      literal_val = this_node;
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

  //  escape-char_t ::= '\"' | "\'" | "\?" | "\\" | "\a" | "\b" | "\f" | "\n" | "\r" | "\t" | "\v" | "\x" hex-digit hex-digit

  std::bitset<128> escape_char_set;

  uint64_t to_hex(char_t c) {
    return (c <= '9' ? c : (c-'A'+10)) & 0x0f;
  }

  bool escape_char() {
    const char_t *save = src;
    if (is_chr('\\') && is_chr(escape_char_set)) {
      if (is_chr('x') && hex_digit() && hex_digit()) {
        literal_val = to_hex(src[-2]) * 16 + to_hex(src[-1]);
        return true;
      } else {
        char_t c = src[-1];
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
    const char_t *save = src;
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
    const char_t *save = src;
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
    const char_t *save = src;
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
    const char_t *save = src;
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

  //  char_t-literal ::= "'" ([#x20-#x26#x28-#x5B#x5D-#x7E] | escape-char_t)+ "'"
  std::bitset<128> char_literal_set;

  bool char_literal() {
    const char_t *save = src;
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
  //    | char_t-literal)
  bool integer_literal(data_type_t type) {
    const char_t *save = src;
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
  bool float_literal(data_type_t type) {
    const char_t *save = src;
    int sign = 0;
    if (is_chr('+') || is_chr('-')) {
      sign = src[-1] == '-';
    }
    if (hex_literal() || binary_literal()) {
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
      if (type == data_type_t::dt_float) {
        union {
          uint32_t u32;
          float f32;
        } u;
        u.f32 = (float)val;
        literal_val = u.u32;
      } else {
        union {
          uint64_t u64;
          double f64;
        } u;
        u.f64 = (float)val;
        literal_val = u.u64;
      }
      skip_ws();
      return true;
    }
    src = save;
    return false;
  }

  //  string-literal ::= ('"' ([#x20-#x21#x23-#x5B#x5D-#x7E#xA0-#xD7FF#xE000-#xFFFD#x010000-#x10FFFF]
  //    | escape-char_t | "\u" hex-digit hex-digit hex-digit hex-digit
  //    | "\U" hex-digit hex-digit hex-digit hex-digit hex-digit hex-digit)* '"')+
  bool string_literal() {
    const char_t *save = src;
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
      literal_val = ddl->add_string(string_val);
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
    const char_t *save = src;
    if (identifier()) {
      switch (identifier_val[0]) {
        case 'b': {
          if (identifier_val == "bool") { literal_val = (uint64_t)data_type_t::dt_bool; return true; }
        } break;
        case 'i': {
          if (identifier_val == "int8") { literal_val = (uint64_t)data_type_t::dt_int8; return true; }
          if (identifier_val == "int16") { literal_val = (uint64_t)data_type_t::dt_int16; return true; }
          if (identifier_val == "int32") { literal_val = (uint64_t)data_type_t::dt_int32; return true; }
          if (identifier_val == "int64") { literal_val = (uint64_t)data_type_t::dt_int64; return true; }
        } break;
        case 'u': {
          if (identifier_val == "unsigned_int8") { literal_val = (uint64_t)data_type_t::dt_unsigned_int8; return true; }
          if (identifier_val == "unsigned_int16") { literal_val = (uint64_t)data_type_t::dt_unsigned_int16; return true; }
          if (identifier_val == "unsigned_int32") { literal_val = (uint64_t)data_type_t::dt_unsigned_int32; return true; }
          if (identifier_val == "unsigned_int64") { literal_val = (uint64_t)data_type_t::dt_unsigned_int64; return true; }
        } break;
        case 'f': {
          if (identifier_val == "float") { literal_val = (uint64_t)data_type_t::dt_float; return true; }
        } break;
        default: {
          if (identifier_val == "double") { literal_val = (uint64_t)data_type_t::dt_double; return true; }
          if (identifier_val == "string") { literal_val = (uint64_t)data_type_t::dt_string; return true; }
          if (identifier_val == "ref") { literal_val = (uint64_t)data_type_t::dt_ref; return true; }
          if (identifier_val == "type") { literal_val = (uint64_t)data_type_t::dt_type; return true; }
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

  template <class _Fn> bool data_array(node_index this_node, data_type_t type, size_t group_size, bool grouped, _Fn literal) {
    size_t size = data_type_size(type);
    data_val.resize(0);
    if (grouped) {
      while (is('{')) {
        size_t gs = 0;
        while (literal(type)) {
          for (size_t i = 0; i != size; ++i) {
            data_val.push_back((uint8_t)literal_val);
            literal_val >>= 8;
          }
          gs++;
          if (!is(',')) break;
        }
        if (!expect('}')) return false;
        if (gs != group_size) return err("data array list size mismatch");
        if (!is(',')) break;
      }
    } else {
      while (literal(type)) {
        for (size_t i = 0; i != size; ++i) {
          data_val.push_back((uint8_t)literal_val);
          literal_val >>= 8;
        }
        if (!is(',')) break;
      }
    }
    size_t len = data_val.size();
    ddl->set_data(this_node, data_val);
    ddl->set_data_type(this_node, type);
    ddl->set_data_group_size(this_node, group_size);
    return true;
  }

  bool data_array(node_index ni, data_type_t type, size_t group_size, bool grouped, node_index parent) {
    switch (type) {
      case data_type_t::dt_bool: {
        return data_array(ni, type, group_size, grouped, [=](data_type_t type){ return bool_literal(); });
      } break;
      case data_type_t::dt_int8:
      case data_type_t::dt_int16:
      case data_type_t::dt_int32:
      case data_type_t::dt_int64:
      case data_type_t::dt_unsigned_int8:
      case data_type_t::dt_unsigned_int16:
      case data_type_t::dt_unsigned_int32:
      case data_type_t::dt_unsigned_int64: {
        return data_array(ni, type, group_size, grouped, [=](data_type_t type){ return integer_literal(type); });
      } break;
      case data_type_t::dt_float:
      case data_type_t::dt_double: {
        return data_array(ni, type, group_size, grouped, [=](data_type_t type){ return float_literal(type); });
      } break;
      case data_type_t::dt_string: {
        return data_array(ni, type, group_size, grouped, [=](data_type_t type){ return string_literal(); });
      } break;
      case data_type_t::dt_ref: {
        return data_array(ni, type, group_size, grouped, [=](data_type_t type){ return reference(parent); });
      } break;
      case data_type_t::dt_type: {
        return data_array(ni, type, group_size, grouped, [=](data_type_t type){ return data_type(); });
      } break;
    }
    return false;
  }

  //  property ::= identifier "=" (bool-literal | integer-literal | float-literal
  //    | string-literal | reference | data-type)

  bool property(node_index parent) {
    const char_t *save = src;
    if (identifier() && is('=')) {
      push_string(data_val, identifier_val);
      if (bool_literal()) {
        data_val.push_back((uint8_t)data_type_t::dt_bool);
        data_val.push_back((uint8_t)literal_val);
        return true;
      } else if (float_literal(data_type_t::dt_double)) {
        data_val.push_back((uint8_t)data_type_t::dt_double);
        push_val(data_val, literal_val);
        return true;
      } else if (integer_literal(data_type_t::dt_int64)) {
        data_val.push_back((uint8_t)data_type_t::dt_int64);
        push_val(data_val, literal_val);
        return true;
      } else if (string_literal()) {
        data_val.push_back((uint8_t)data_type_t::dt_string);
        push_val(data_val, (data_index)literal_val);
        return true;
      } else if (reference(parent)) {
        data_val.push_back((uint8_t)data_type_t::dt_ref);
        push_val(data_val, (node_index)literal_val);
        return true;
      } else if (data_type()) {
        data_val.push_back((uint8_t)data_type_t::dt_type);
        data_val.push_back((uint8_t)literal_val);
        return true;
      }
    }
    src = save;
    return false;
  }
  
  //  structure ::=
  //    data-type (name? "{" data-list? "}" | "[" integer-literal "]" name? "{" data-array-list? "}")
  //    | identifier name? ("(" (property ("," property)*)? ")")? "{" structure* "}"

  bool structure(node_index parent) {
    const char_t *save = src;
    if (data_type()) {
      node_index this_node = ddl->add_child(parent);
      data_type_t type = (data_type_t)literal_val;
      ddl->set_data_type(this_node, type);
      if (is('[')) {
        // eg. float [5] $fred {{...}, {...}, ...}
        size_t group_size = 0;
        if (!integer_literal(data_type_t::dt_unsigned_int64)) {
          return err("expected integer literal");
        }
        group_size = (size_t)literal_val;
        if (!expect(']')) return false;

        if (name()) {
          ddl->set_name(this_node, identifier_val);
        }

        if (!expect('{')) return false;
        if (data_array(this_node, type, group_size, true, parent)) {
        }
        if (!expect('}')) return false;
        return true;
      } else {
        // eg. float %fred {0.1}
        if (name()) {
          ddl->set_name(this_node, identifier_val);
        }

        if (!expect('{')) return false;
        if (data_array(this_node, type, 0, false, parent)) {
        }
        if (!expect('}')) return false;
        return true;
      }
    } else if (identifier()) {
      // eg. GeometryNode 
      node_index this_node = ddl->add_child(parent);
      ddl->set_kind(this_node, identifier_val);
      if (name()) {
        ddl->set_name(this_node, identifier_val);
      }

      if (is('(')) {
        data_val.resize(0);
        while (property(parent)) {
          if (!is(',')) break;
        }
        if (!expect(')')) return false;
        data_val.push_back(0);
        ddl->set_props(parent, data_val);
      }

      if (!expect('{')) return false;
      while (structure(this_node)) {
      }
      if (!expect('}')) return false;

      return true;
    }
    src = save;
    return false;
  }

  //  file ::= structure*

  bool parse_file() {
    if (src > end) return false;

    while (structure(0)) {
    }

    return src == end;
  }

  static void push_string(std::vector<uint8_t> &dest, const std::string &src) {
    dest.insert(dest.end(), src.begin(), src.end());
    dest.push_back(0);
  }

  template<class _Type> static void push_val(std::vector<uint8_t> &dest, _Type t) {
    size_t size = sizeof(t);
    while (size--) { dest.push_back((uint8_t)t); t >>= 8; }
  }
public:
  parser(_Err err_func) : err_func(err_func) {
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
  }

  bool parse(file *ddl, const char_t *begin, const char_t *end) {
    this->ddl = ddl;
    this->begin = begin;
    this->end = end;
    src = begin;
    line_number = 1;
    skip_ws();
    return parse_file();
  }
};

} // namespace openddl

#endif

