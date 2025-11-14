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
logic s1_in1_x_in2_negative;
logic s1_in1_x_in2_is_infinite, s1_in1_x_in2_is_zero;
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
    s1_in2_positive = ~s1_in2_init.sign;

    s1_in3_iszero = (s1_in3_init.exponent == '0) & (s1_in3_init.mantissa == '0);
    s1_in3_isinfinite = (s1_in3_init.exponent == '1) & (s1_in3_init.mantissa == '0);
    s1_in3_isqnan = (s1_in3_init.exponent == '1) & (s1_in3_init.mantissa != '0) & s1_in3_init.mantissa[22];
    s1_in3_issnan = (s1_in3_init.exponent == '1) & (s1_in3_init.mantissa != '0) & ~s1_in3_init.mantissa[22];
    s1_in3_isdenorm = (s1_in3_init.exponent == '0) & (s1_in3_init.mantissa != '0);
    s1_in3_positive = ~s1_in3_init.sign;

    s1_in1_x_in2_negative = s1_in1_init.sign ^ s1_in2_init.sign;
    s1_in1_x_in2_is_infinite = (s1_in1_isinfinite | s1_in2_isinfinite) & ~(s1_in1_iszero | s1_in1_isdenorm | s1_in2_iszero | s1_in2_isdenorm);
    s1_in1_x_in2_is_zero = (s1_in1_iszero | s1_in1_isdenorm | s1_in2_iszero | s1_in2_isdenorm) & ~s1_in1_x_in2_is_infinite;
end
//flush to zero and invalid input handling
logic s1_input_is_invalid;
logic s1_input_is_flushed;
assign s1_input_is_invalid = s1_in1_issnan | s1_in2_issnan | s1_in3_issnan | 
((s1_in1_iszero | s1_in1_isdenorm) & s1_in2_isinfinite) | ((s1_in2_iszero | s1_in2_isdenorm) & s1_in1_isinfinite) |
(((s1_in1_isinfinite | s1_in2_isinfinite) & s1_in3_isinfinite) & (s1_in1_x_in2_negative ^ s1_in3_positive));
assign s1_input_is_flushed = s1_in1_isdenorm | s1_in2_isdenorm | s1_in3_isdenorm;

logic s1_result_is_pos_infinity, s1_result_is_neg_infinity, s1_result_is_pos_zero, s1_result_is_neg_zero;
assign s1_result_is_pos_infinity = ((s1_in1_x_in2_is_infinite & ~s1_in1_x_in2_negative) & ~(s1_in3_isinfinite & ~s1_in3_positive)) |
(s1_in3_isinfinite & s1_in3_positive & ~(s1_in1_x_in2_is_infinite & s1_in1_x_in2_negative));
assign s1_result_is_neg_infinity = ((s1_in1_x_in2_is_infinite & s1_in1_x_in2_negative) & ~(s1_in3_isinfinite & s1_in3_positive)) |
(s1_in3_isinfinite & ~s1_in3_positive & ~(s1_in1_x_in2_is_infinite & ~s1_in1_x_in2_negative));

assign s1_result_is_pos_zero = s1_in1_x_in2_is_zero & s1_in3_iszero & ((rounding_mode != RDN) | s1_in3_positive);

assign s1_result_is_neg_zero = s1_in1_x_in2_is_zero & s1_in3_iszero & 
((s1_in1_x_in2_negative & ~s1_in3_positive) | (~s1_in1_x_in2_negative & ~s1_in3_positive & (rounding_mode == RDN)) |
(s1_in1_x_in2_negative & s1_in3_positive & (rounding_mode == RDN)));

fp_32b_t s2_special_result, s2_in1, s2_in2, s2_in3;
logic s2_input_is_invalid;
logic s2_input_is_flushed;
logic s2_special_case;
logic s2_valid_data_in;
logic[2:0] s2_rounding_mode;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s2_in1 <= '0;
        s2_in2 <= '0;
        s2_in3 <= '0;
        s2_input_is_invalid <= 0;
        s2_input_is_flushed <= 0;
        s2_valid_data_in <= 0;
        s2_rounding_mode <= '0;
        s2_special_case <= 0;
        s2_special_result <= '0;
    end else begin
        s2_in1 <= s1_in1_init;
        s2_in2 <= s1_in2_init;
        s2_in3 <= s1_in3_init;
        s2_input_is_invalid <= s1_input_is_invalid;
        s2_input_is_flushed <= s1_input_is_flushed;
        s2_valid_data_in <= valid_data_in;
        s2_rounding_mode <= rounding_mode;

        //Special cases:
        //propagate qnan
        if(s1_in1_isqnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in1_init;
        end else if(s1_in2_isqnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in2_init;
        end else if(s1_in3_isqnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in3_init;
        
        //convert snan to qnan
        end else if(s1_in1_issnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in1_init | 32'b00000000010000000000000000000000;

        end else if(s1_in2_issnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in2_init | 32'b00000000010000000000000000000000;

        end else if(s1_in3_issnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in3_init | 32'b00000000010000000000000000000000;
        end else if(s1_result_is_pos_infinity) begin
            s2_special_case <= 1;
            s2_special_result <= 32'b01111111100000000000000000000000;
        end else if(s1_result_is_neg_infinity) begin
            s2_special_case <= 1;
            s2_special_result <= 32'b11111111100000000000000000000000;
        end else if(s1_result_is_pos_zero) begin
            s2_special_case <= 1;
            s2_special_result <= 32'b00000000000000000000000000000000;
        end else if(s1_result_is_neg_zero) begin
            s2_special_case <= 1;
            s2_special_result <= 32'b10000000000000000000000000000000;
        end else begin
            s2_special_case <= 0;
            //this doesnt matter, but its there to show intent that the special result doesnt get propagated since special case is zero
            s2_special_result <= '0;
        end
    end
end

endmodule