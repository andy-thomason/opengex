#include "openddl.h"

#include <iostream>
#include <fstream>

static char buf[0x10000];

int main() {
  std::ifstream fs("test.odx");
  fs.seekg(0, std::ios_base::end);
  int size = (int)fs.tellg();
  fs.seekg(0);
  fs.read(buf, size);
  openddl <> oddl(buf, buf + size);
}

