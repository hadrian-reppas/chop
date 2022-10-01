
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <wchar.h>
#include <locale.h>
void hopf_6d_61_69_6e_0_0();
void hopf_70_75_74_6c_6e_73_0_0(uint8_t*);
void hopf_6d_61_69_6e_0_0() {
  uint8_t* hopv_0 = (uint8_t*) "Hello, World!";
  hopf_70_75_74_6c_6e_73_0_0(hopv_0);
}
void hopf_70_75_74_6c_6e_73_0_0(uint8_t* hopv_0) {
  printf("%s\n", (char*) hopv_0);
}
int main(int argc, char** argv) {
  setlocale(LC_ALL, "");
  int64_t code = 0;
  hopf_6d_61_69_6e_0_0();
  return code;
}
