
#include "../src/openddl.h"
#include "../src/opengex.h"

#include <iostream>
#include <fstream>
#include <vector>

void test_openddl() {
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
  std::cout << ddl;
}

int main() {
  test_openddl();
}

