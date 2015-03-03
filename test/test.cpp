
#include "../src/openddl.h"
#include "../src/opengex.h"

#include <iostream>
#include <fstream>
#include <vector>

auto err = [](const char *err, int line_number) {
  std::cout << err << " @ line " << line_number << "\n";
  return false;
};

bool test_fwd_refs() {
  openddl::file ddl;
  openddl::parser<char, decltype(err)> parser(err);

  const char test[] = "X $X { ref %x { $fwd } } Y $fwd { }";
  if (!parser.parse(&ddl, std::begin(test), std::end(test))) {
    return false;
  }
  std::cout << ddl << "\n";

  openddl::struct_index si = (openddl::struct_index)ddl.get_datum(ddl.get_child(ddl.get_global("$X"), "%x"), 0);
  return true;
}

void test_openddl() {

  std::ifstream fs("../../test/test.odx");
  fs.seekg(0, std::ios_base::end);
  size_t size = (size_t)fs.tellg();
  fs.seekg(0);
  std::vector<char> buf(size);
  fs.read(buf.data(), size);
  openddl::file ddl;
  openddl::parser<char, decltype(err)> parser(err);
  parser.parse(&ddl, buf.data(), buf.data() + buf.size());
  std::cout << ddl;

  ddl.for_structs_by_kind(0, "GeometryObject",
    [&](openddl::struct_index ni) {
      std::cout << ni << " " << ddl.get_name(ni) << "\n";
    }
  );
}

void test_opengex() {
  std::ifstream fs("../../test/test.odx");
  fs.seekg(0, std::ios_base::end);
  size_t size = (size_t)fs.tellg();
  fs.seekg(0);
  std::vector<char> buf(size);
  fs.read(buf.data(), size);
  openddl::file ddl;
  auto err = [&](const char *err, int line_number) {
    std::cout << err << " @ line " << line_number << "\n";
    return false;
  };
  openddl::parser<char, decltype(err)> parser(err);
  parser.parse(&ddl, buf.data(), buf.data() + buf.size());
}

int main() {
  test_fwd_refs();

  //test_openddl();
}

