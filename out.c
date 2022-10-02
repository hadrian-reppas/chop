
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <wchar.h>
#include <locale.h>
void hf_3d_3d_1_0(int64_t, int64_t, uint8_t*);
void hf_66_69_62_0_0(int64_t, int64_t*);
void hf_61_73_73_65_72_74_0_0(uint8_t);
void hf_2b_4_0(int64_t, int64_t, int64_t*);
void hf_6d_61_69_6e_0_0();
void hf_70_75_74_6c_6e_0_0(int64_t);
void hf_26_2_0(uint8_t, uint8_t, uint8_t*);
void hf_2d_4_0(int64_t, int64_t, int64_t*);
void hf_21_3d_1_0(int64_t, int64_t, uint8_t*);
void hf_3d_3d_1_0(int64_t hv0, int64_t hv1, uint8_t* hr0) {
    *hr0 = hv0 == hv1;
}
void hf_66_69_62_0_0(int64_t hv0, int64_t* hr0) {
    int64_t hv13;
    int64_t hv14 = 0;
    uint8_t hv15;
    hf_21_3d_1_0(hv0, hv14, &hv15);
    int64_t hv16 = 1;
    uint8_t hv17;
    hf_21_3d_1_0(hv0, hv16, &hv17);
    uint8_t hv18;
    hf_26_2_0(hv15, hv17, &hv18);
    if (hv18) {
        int64_t hv19 = 1;
        int64_t hv20;
        hf_2d_4_0(hv0, hv19, &hv20);
        int64_t hv21;
        hf_66_69_62_0_0(hv20, &hv21);
        int64_t hv22 = 2;
        int64_t hv23;
        hf_2d_4_0(hv0, hv22, &hv23);
        int64_t hv24;
        hf_66_69_62_0_0(hv23, &hv24);
        int64_t hv25;
        hf_2b_4_0(hv21, hv24, &hv25);
        hv13 = hv25;
    } else {
        hv13 = hv0;
    }
    *hr0 = hv13;
}
void hf_61_73_73_65_72_74_0_0(uint8_t hv0) {
}
void hf_2b_4_0(int64_t hv0, int64_t hv1, int64_t* hr0) {
    *hr0 = hv0 + hv1;
}
void hf_6d_61_69_6e_0_0() {
    int64_t hv0 = 0;
    int64_t hv1;
    hf_66_69_62_0_0(hv0, &hv1);
    int64_t hv2 = 0;
    uint8_t hv3;
    hf_3d_3d_1_0(hv1, hv2, &hv3);
    if (!hv3) {
        printf("assertion failed at tests/fib.hop:12:16\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv4 = 1;
    int64_t hv5;
    hf_66_69_62_0_0(hv4, &hv5);
    int64_t hv6 = 1;
    uint8_t hv7;
    hf_3d_3d_1_0(hv5, hv6, &hv7);
    if (!hv7) {
        printf("assertion failed at tests/fib.hop:13:16\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv8 = 2;
    int64_t hv9;
    hf_66_69_62_0_0(hv8, &hv9);
    int64_t hv10 = 1;
    uint8_t hv11;
    hf_3d_3d_1_0(hv9, hv10, &hv11);
    if (!hv11) {
        printf("assertion failed at tests/fib.hop:14:16\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv12 = 3;
    int64_t hv13;
    hf_66_69_62_0_0(hv12, &hv13);
    int64_t hv14 = 2;
    uint8_t hv15;
    hf_3d_3d_1_0(hv13, hv14, &hv15);
    if (!hv15) {
        printf("assertion failed at tests/fib.hop:15:16\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv16 = 4;
    int64_t hv17;
    hf_66_69_62_0_0(hv16, &hv17);
    int64_t hv18 = 3;
    uint8_t hv19;
    hf_3d_3d_1_0(hv17, hv18, &hv19);
    if (!hv19) {
        printf("assertion failed at tests/fib.hop:16:16\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv20 = 5;
    int64_t hv21;
    hf_66_69_62_0_0(hv20, &hv21);
    int64_t hv22 = 5;
    uint8_t hv23;
    hf_3d_3d_1_0(hv21, hv22, &hv23);
    if (!hv23) {
        printf("assertion failed at tests/fib.hop:17:16\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv24 = 6;
    int64_t hv25;
    hf_66_69_62_0_0(hv24, &hv25);
    int64_t hv26 = 8;
    uint8_t hv27;
    hf_3d_3d_1_0(hv25, hv26, &hv27);
    if (!hv27) {
        printf("assertion failed at tests/fib.hop:18:16\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv28 = 7;
    int64_t hv29;
    hf_66_69_62_0_0(hv28, &hv29);
    int64_t hv30 = 13;
    uint8_t hv31;
    hf_3d_3d_1_0(hv29, hv30, &hv31);
    if (!hv31) {
        printf("assertion failed at tests/fib.hop:19:17\n");
        exit(EXIT_FAILURE);
    }
    int64_t hv32 = 0;
    int64_t hv33 = 10;
    for (int64_t hv34 = hv32; hv34 < hv33; hv34++) {
        int64_t hv35;
        hf_66_69_62_0_0(hv34, &hv35);
        hf_70_75_74_6c_6e_0_0(hv35);
    }
}
void hf_70_75_74_6c_6e_0_0(int64_t hv0) {
    printf("%" PRId64 "\n", hv0);
}
void hf_26_2_0(uint8_t hv0, uint8_t hv1, uint8_t* hr0) {
    *hr0 = hv0 && hv1;
}
void hf_2d_4_0(int64_t hv0, int64_t hv1, int64_t* hr0) {
    *hr0 = hv0 - hv1;
}
void hf_21_3d_1_0(int64_t hv0, int64_t hv1, uint8_t* hr0) {
    *hr0 = hv0 != hv1;
}
int main(int argc, char** argv) {
    setlocale(LC_ALL, "");
    int64_t code = 0;
    hf_6d_61_69_6e_0_0();
    return code;
}
