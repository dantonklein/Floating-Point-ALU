import fp_pkg::*;

//(in1 * in2) + in3
module fp_fused_mult_add_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2, in3
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation,
    output logic valid_data_out
);
//Stage 1: Denorm, NaN, Zero, Infinity processing
//input flag handling
fp_32b_t s1_in1_init;
fp_32b_t s1_in2_init;
fp_32b_t s1_in3_init;
assign s1_in1_init = in1;
assign s1_in2_init = in2;
assign s1_in3_init = in3;

//special case handling
logic s1_in1_iszero, s1_in2_iszero, s1_in3_iszero;
logic s1_in1_isinfinite, s1_in2_isinfinite, s1_in3_isinfinite;
logic s1_in1_isqnan, s1_in2_isqnan, s1_in3_isqnan;
logic s1_in1_issnan, s1_in2_issnan, s1_in3_issnan;
logic s1_in1_isdenorm, s1_in2_isdenorm, s1_in3_isdenorm;
logic s1_in1_finite, s1_in2_finite, s1_in3_finite;
logic s1_in1_positive, s1_in2_positive, s1_in3_positive;
//special cases for input
always_comb begin
    s1_in1_iszero = (s1_in1_init.exponent == '0) & (s1_in1_init.mantissa == '0);
    s1_in1_isinfinite = (s1_in1_init.exponent == '1) & (s1_in1_init.mantissa == '0);
    s1_in1_isqnan = (s1_in1_init.exponent == '1) & (s1_in1_init.mantissa != '0) & s1_in1_init.mantissa[22];
    s1_in1_issnan = (s1_in1_init.exponent == '1) & (s1_in1_init.mantissa != '0) & ~s1_in1_init.mantissa[22];
    s1_in1_isdenorm = (s1_in1_init.exponent == '0) & (s1_in1_init.mantissa != '0);
    s1_in1_finite = ~s1_in1_iszero & ~s1_in1_isinfinite & ~s1_in1_isqnan & ~s1_in1_issnan & ~s1_in1_isdenorm;
    s1_in1_positive = ~s1_in1_init.sign;

    s1_in2_iszero = (s1_in2_init.exponent == '0) & (s1_in2_init.mantissa == '0);
    s1_in2_isinfinite = (s1_in2_init.exponent == '1) & (s1_in2_init.mantissa == '0);
    s1_in2_isqnan = (s1_in2_init.exponent == '1) & (s1_in2_init.mantissa != '0) & s1_in2_init.mantissa[22];
    s1_in2_issnan = (s1_in2_init.exponent == '1) & (s1_in2_init.mantissa != '0) & ~s1_in2_init.mantissa[22];
    s1_in2_isdenorm = (s1_in2_init.exponent == '0) & (s1_in2_init.mantissa != '0);
    s1_in2_finite = ~s1_in2_iszero & ~s1_in2_isinfinite & ~s1_in2_isqnan & ~s1_in2_issnan & ~s1_in2_isdenorm;
    s1_in2_positive = ~s1_in2_init.sign;

    s1_in3_iszero = (s1_in3_init.exponent == '0) & (s1_in3_init.mantissa == '0);
    s1_in3_isinfinite = (s1_in3_init.exponent == '1) & (s1_in3_init.mantissa == '0);
    s1_in3_isqnan = (s1_in3_init.exponent == '1) & (s1_in3_init.mantissa != '0) & s1_in3_init.mantissa[22];
    s1_in3_issnan = (s1_in3_init.exponent == '1) & (s1_in3_init.mantissa != '0) & ~s1_in3_init.mantissa[22];
    s1_in3_isdenorm = (s1_in3_init.exponent == '0) & (s1_in3_init.mantissa != '0);
    s1_in3_finite = ~s1_in3_iszero & ~s1_in3_isinfinite & ~s1_in3_isqnan & ~s1_in3_issnan & ~s1_in3_isdenorm;
    s1_in3_positive = ~s1_in3_init.sign;

end
//flush to zero and invalid input handling
logic s1_input_is_invalid;
logic s1_input_is_flushed;
assign s1_input_is_invalid = s1_in1_issnan | s1_in2_issnan | s1_in3_issnan | 
((s1_in1_iszero | s1_in1_isdenorm) & s1_in2_isinfinite) | ((s1_in2_iszero | s1_in2_isdenorm) & s1_in1_isinfinite) |
((s1_in1_isinfinite & s1_in2_isinfinite & s1_in3_isinfinite) & 
((s1_in1_init.sign & s1_in2_init.sign & ~s1_in3_init.sign) | 
((s1_in1_init.sign ^ s1_in2_init.sign) & s1_in3_init.sign) | 
(~s1_in1_init.sign & ~s1_in2_init.sign & ~s1_in3_init.sign))) |
();
assign s1_input_is_flushed = s1_in1_isdenorm | s1_in2_isdenorm | s1_in3_isdenorm;

fp_32b_t s2_special_result, s2_in1, s2_in2;
logic s2_input_is_invalid;
logic s2_input_is_flushed;
logic s2_special_case;
logic s2_valid_data_in;
logic[2:0] s2_rounding_mode;
endmodule