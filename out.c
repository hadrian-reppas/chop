
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <wchar.h>
#include <locale.h>
struct hopt_63_6f_6d_70_6c_65_78 {
    double hf_72_65_61_6c;
    double hf_69_6d_61_67;
};
void hf_70_75_74_63_0_0(int64_t);
void hf_70_75_74_1_0(double);
void hf_70_75_74_5_0(struct hopt_63_6f_6d_70_6c_65_78);
void hf_6c_6e_0_0();
void hf_2e_72_65_61_6c_0_0(struct hopt_63_6f_6d_70_6c_65_78, double*);
void hf_2e_69_6d_61_67_0_0(struct hopt_63_6f_6d_70_6c_65_78, double*);
void hf_2b_8_0(double, double, double*);
void hf_2b_11_0(struct hopt_63_6f_6d_70_6c_65_78, struct hopt_63_6f_6d_70_6c_65_78, struct hopt_63_6f_6d_70_6c_65_78*);
void hf_70_75_74_73_0_0(uint8_t*);
void hf_63_6f_6d_70_6c_65_78_0_0(double, double, struct hopt_63_6f_6d_70_6c_65_78*);
void hf_6d_61_69_6e_0_0();
void hf_70_75_74_63_0_0(int64_t hv0) {
    printf("%lc", (wint_t) hv0);
}
void hf_70_75_74_1_0(double hv0) {
    printf("%.15g", hv0);
}
void hf_70_75_74_5_0(struct hopt_63_6f_6d_70_6c_65_78 hv0) {
    double hv1;
    hf_2e_72_65_61_6c_0_0(hv0, &hv1);
    hf_70_75_74_1_0(hv1);
    uint8_t* hv2 = (uint8_t*) "\x20\x2b\x20";
    hf_70_75_74_73_0_0(hv2);
    double hv3;
    hf_2e_69_6d_61_67_0_0(hv0, &hv3);
    hf_70_75_74_1_0(hv3);
    int64_t hv4 = 106;
    hf_70_75_74_63_0_0(hv4);
}
void hf_6c_6e_0_0() {
    printf("\n");
}void hf_2e_72_65_61_6c_0_0(struct hopt_63_6f_6d_70_6c_65_78 hv0, double* hr0) {
    *hr0 = hv0.hf_72_65_61_6c;
}
void hf_2e_69_6d_61_67_0_0(struct hopt_63_6f_6d_70_6c_65_78 hv0, double* hr0) {
    *hr0 = hv0.hf_69_6d_61_67;
}
void hf_2b_8_0(double hv0, double hv1, double* hr0) {
    *hr0 = hv0 + hv1;
}
void hf_2b_11_0(struct hopt_63_6f_6d_70_6c_65_78 hv0, struct hopt_63_6f_6d_70_6c_65_78 hv1, struct hopt_63_6f_6d_70_6c_65_78* hr0) {
    double hv2;
    hf_2e_72_65_61_6c_0_0(hv0, &hv2);
    double hv3;
    hf_2e_72_65_61_6c_0_0(hv1, &hv3);
    double hv4;
    hf_2b_8_0(hv2, hv3, &hv4);
    double hv5;
    hf_2e_69_6d_61_67_0_0(hv0, &hv5);
    double hv6;
    hf_2e_69_6d_61_67_0_0(hv1, &hv6);
    double hv7;
    hf_2b_8_0(hv5, hv6, &hv7);
    struct hopt_63_6f_6d_70_6c_65_78 hv8;
    hf_63_6f_6d_70_6c_65_78_0_0(hv4, hv7, &hv8);
    *hr0 = hv8;
}
void hf_70_75_74_73_0_0(uint8_t* hv0) {
    printf("%s", (char*) hv0);
}
void hf_63_6f_6d_70_6c_65_78_0_0(double hv0, double hv1, struct hopt_63_6f_6d_70_6c_65_78* hr0) {
    hr0->hf_72_65_61_6c = hv0;
    hr0->hf_69_6d_61_67 = hv1;
}
void hf_6d_61_69_6e_0_0() {
    double hv0 = 1.0;
    double hv1 = 3.5;
    struct hopt_63_6f_6d_70_6c_65_78 hv2;
    hf_63_6f_6d_70_6c_65_78_0_0(hv0, hv1, &hv2);
    double hv3 = -0.5;
    double hv4 = 2.1;
    struct hopt_63_6f_6d_70_6c_65_78 hv5;
    hf_63_6f_6d_70_6c_65_78_0_0(hv3, hv4, &hv5);
    struct hopt_63_6f_6d_70_6c_65_78 hv6;
    hf_2b_11_0(hv2, hv5, &hv6);
    hf_70_75_74_5_0(hv6);
    hf_6c_6e_0_0();
}
int main(int argc, char** argv) {
    setlocale(LC_ALL, "");
    int64_t code = 0;
    hf_6d_61_69_6e_0_0();
    return code;
}
