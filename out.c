
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <wchar.h>
#include <locale.h>
struct hopt_6c_69_73_74 {
    int64_t hf_76_61_6c_75_65;
    struct hopt_6c_69_73_74* hf_6e_65_78_74;
};
void hf_7e_0_0(struct hopt_6c_69_73_74*);
void hf_7e_0_1(struct hopt_6c_69_73_74);
void hf_6c_69_73_74_0_0(int64_t, struct hopt_6c_69_73_74*, struct hopt_6c_69_73_74*);
void hf_3d_3d_1_0(int64_t, int64_t, uint8_t*);
void hf_70_75_74_0_0(int64_t);
void hf_74_6f_5f_6c_69_73_74_5f_70_74_72_1_0(int64_t, struct hopt_6c_69_73_74**);
void hf_40_0_0(struct hopt_6c_69_73_74, struct hopt_6c_69_73_74*, struct hopt_6c_69_73_74**);
void hf_6c_6e_0_0();
void hf_70_75_74_63_0_0(int64_t);
void hf_2e_2e_76_61_6c_75_65_0_0(struct hopt_6c_69_73_74, struct hopt_6c_69_73_74*, int64_t*);
void hf_70_75_74_73_0_0(uint8_t*);
void hf_2e_6e_65_78_74_0_0(struct hopt_6c_69_73_74, struct hopt_6c_69_73_74**);
void hf_2a_9_0(struct hopt_6c_69_73_74*, struct hopt_6c_69_73_74*);
void hf_70_72_69_6e_74_0_0(struct hopt_6c_69_73_74*);
void hf_6d_61_69_6e_0_0();
void hf_74_6f_5f_69_6e_74_2_0(struct hopt_6c_69_73_74*, int64_t*);
void hf_2e_0_0(struct hopt_6c_69_73_74*, struct hopt_6c_69_73_74**, struct hopt_6c_69_73_74**);
void hf_7e_0_0(struct hopt_6c_69_73_74* hv0) {
}
void hf_7e_0_1(struct hopt_6c_69_73_74 hv0) {
}
void hf_6c_69_73_74_0_0(int64_t hv0, struct hopt_6c_69_73_74* hv1, struct hopt_6c_69_73_74* hr0) {
    hr0->hf_76_61_6c_75_65 = hv0;
    hr0->hf_6e_65_78_74 = hv1;
}
void hf_3d_3d_1_0(int64_t hv0, int64_t hv1, uint8_t* hr0) {
    *hr0 = hv0 == hv1;
}
void hf_70_75_74_0_0(int64_t hv0) {
    printf("%" PRId64, hv0);
}
void hf_74_6f_5f_6c_69_73_74_5f_70_74_72_1_0(int64_t hv0, struct hopt_6c_69_73_74** hr0) {
    *hr0 = (struct hopt_6c_69_73_74*) hv0;
}
void hf_40_0_0(struct hopt_6c_69_73_74 hv0, struct hopt_6c_69_73_74* hr0, struct hopt_6c_69_73_74** hr1) {
}
void hf_6c_6e_0_0() {
    printf("\n");
}void hf_70_75_74_63_0_0(int64_t hv0) {
    printf("%lc", (wint_t) hv0);
}
void hf_2e_2e_76_61_6c_75_65_0_0(struct hopt_6c_69_73_74 hv0, struct hopt_6c_69_73_74* hr0, int64_t* hr1) {
    *hr0 = hv0;
    *hr1 = hv0.hf_76_61_6c_75_65;
}
void hf_70_75_74_73_0_0(uint8_t* hv0) {
    printf("%s", (char*) hv0);
}
void hf_2e_6e_65_78_74_0_0(struct hopt_6c_69_73_74 hv0, struct hopt_6c_69_73_74** hr0) {
    *hr0 = hv0.hf_6e_65_78_74;
}
void hf_2a_9_0(struct hopt_6c_69_73_74* hv0, struct hopt_6c_69_73_74* hr0) {
    *hr0 = *hv0;
}
void hf_70_72_69_6e_74_0_0(struct hopt_6c_69_73_74* hv0) {
    struct hopt_6c_69_73_74* hv7;
    struct hopt_6c_69_73_74* hv8;
    hf_2e_0_0(hv0, &hv7, &hv8);
    int64_t hv9;
    hf_74_6f_5f_69_6e_74_2_0(hv8, &hv9);
    int64_t hv10 = 0;
    uint8_t hv11;
    hf_3d_3d_1_0(hv9, hv10, &hv11);
    if (hv11) {
        hf_7e_0_0(hv7);
        uint8_t* hv12 = (uint8_t*) "\x4e\x55\x4c\x4c";
        hf_70_75_74_73_0_0(hv12);
    } else {
        uint8_t* hv13 = (uint8_t*) "\x6c\x69\x73\x74\x28";
        hf_70_75_74_73_0_0(hv13);
        struct hopt_6c_69_73_74 hv14;
        hf_2a_9_0(hv7, &hv14);
        struct hopt_6c_69_73_74 hv15;
        int64_t hv16;
        hf_2e_2e_76_61_6c_75_65_0_0(hv14, &hv15, &hv16);
        hf_70_75_74_0_0(hv16);
        uint8_t* hv17 = (uint8_t*) "\x2c\x20";
        hf_70_75_74_73_0_0(hv17);
        struct hopt_6c_69_73_74* hv18;
        hf_2e_6e_65_78_74_0_0(hv15, &hv18);
        hf_70_72_69_6e_74_0_0(hv18);
        int64_t hv19 = 41;
        hf_70_75_74_63_0_0(hv19);
    }
}
void hf_6d_61_69_6e_0_0() {
    int64_t hv0 = 5;
    int64_t hv1 = 0;
    struct hopt_6c_69_73_74* hv2;
    hf_74_6f_5f_6c_69_73_74_5f_70_74_72_1_0(hv1, &hv2);
    struct hopt_6c_69_73_74 hv3;
    hf_6c_69_73_74_0_0(hv0, hv2, &hv3);
    struct hopt_6c_69_73_74* hv4 = &hv3;
    int64_t hv5 = 10;
    struct hopt_6c_69_73_74 hv6;
    hf_6c_69_73_74_0_0(hv5, hv4, &hv6);
    struct hopt_6c_69_73_74* hv7 = &hv6;
    hf_70_72_69_6e_74_0_0(hv7);
    hf_6c_6e_0_0();
    hf_7e_0_1(hv6);
    hf_7e_0_1(hv3);
}
void hf_74_6f_5f_69_6e_74_2_0(struct hopt_6c_69_73_74* hv0, int64_t* hr0) {
    *hr0 = (int64_t) hv0;
}
void hf_2e_0_0(struct hopt_6c_69_73_74* hv0, struct hopt_6c_69_73_74** hr0, struct hopt_6c_69_73_74** hr1) {
    *hr0 = hv0;
    *hr1 = hv0;
}
int main(int argc, char** argv) {
    setlocale(LC_ALL, "");
    int64_t code = 0;
    hf_6d_61_69_6e_0_0();
    return code;
}
