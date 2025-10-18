import fp_pkg::*;

module fp_multiply_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation,
    output logic valid_data_out
);

//Stage 1: Denorm, NaN, Zero, Infinity processing
//input flag handling
fp_32b_t s1_in1_init;
fp_32b_t s1_in2_init;
assign s1_in1_init = in1;
assign s1_in2_init = in2;

//special case handling
logic s1_in1_iszero, s1_in2_iszero;
logic s1_in1_isinfinite, s1_in2_isinfinite;
logic s1_in1_isqnan, s1_in2_isqnan;
logic s1_in1_issnan, s1_in2_issnan;
logic s1_in1_isdenorm, s1_in2_isdenorm;

//special cases for input
always_comb begin
    //input 1
    s1_in1_iszero = (s1_in1_init.exponent == '0) & (s1_in1_init.mantissa == '0);
    s1_in1_isinfinite = (s1_in1_init.exponent == '1) & (s1_in1_init.mantissa == '0);
    s1_in1_isqnan = (s1_in1_init.exponent == '1) & (s1_in1_init.mantissa != '0) & s1_in1_init.mantissa[22];
    s1_in1_issnan = (s1_in1_init.exponent == '1) & (s1_in1_init.mantissa != '0) & ~s1_in1_init.mantissa[22];
    s1_in1_isdenorm = (s1_in1_init.exponent == '0) & (s1_in1_init.mantissa != '0);

    s1_in2_iszero = (s1_in2_init.exponent == '0) & (s1_in2_init.mantissa == '0);
    s1_in2_isinfinite = (s1_in2_init.exponent == '1) & (s1_in2_init.mantissa == '0);
    s1_in2_isqnan = (s1_in2_init.exponent == '1) & (s1_in2_init.mantissa != '0) & s1_in2_init.mantissa[22];
    s1_in2_issnan = (s1_in2_init.exponent == '1) & (s1_in2_init.mantissa != '0) & ~s1_in2_init.mantissa[22];
    s1_in2_isdenorm = (s1_in2_init.exponent == '0) & (s1_in2_init.mantissa != '0);
end
//flush to zero and invalid input handling
logic s1_input_is_invalid;
logic s1_input_is_flushed;
assign s1_input_is_invalid = s1_in1_issnan | s1_in2_issnan | (s1_in1_iszero & s1_in2_isinfinite) | (s1_in2_iszero & s1_in1_isinfinite);
assign s1_input_is_flushed = s1_in1_isdenorm | s1_in2_isdenorm;

fp_32b_t s2_special_result, s2_in1, s2_in2;
logic s2_input_is_invalid;
logic s2_input_is_flushed;
logic s2_special_case;
logic s2_valid_data_in;
logic[2:0] s2_rounding_mode;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s2_in1 <= '0;
        s2_in2 <= '0;
        s2_input_is_invalid <= 0;
        s2_input_is_flushed <= 0;
        s2_valid_data_in <= 0;
        s2_rounding_mode <= '0;
        s2_special_case <= 0;
        s2_special_result <= '0;
    end else begin
        s2_in1 <= s1_in1_init;
        s2_in2 <= s1_in2_init;
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

        //convert snan to qnan
        end else if(s1_in1_issnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in1_init | 32'b00000000010000000000000000000000;

        end else if(s1_in2_issnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in2_init | 32'b00000000010000000000000000000000;

        //when both infinity
        end else if((s1_in1_isinfinite & s1_in2_iszero) | (s1_in1_iszero & s1_in2_isinfinite)) begin
            //propagate qnan, exception flag is raised 
            s2_special_result <= 32'h7FC00000;

        //infinite input + non-infinite, infinity gets propagated
        end else if(s1_in1_isinfinite | s1_in2_isinfinite) begin
            s2_special_case <= 1;
            s2_special_result <= {s1_in1_init.sign ^ s1_in2_init.sign,31'h7F800000};

        //if either zeroes or denormal, different cases
        end else if(s1_in1_iszero | s1_in1_isdenorm | s1_in2_iszero | s1_in2_isdenorm) begin
            s2_special_case <= 1;
            if(rounding_mode == RDN) s2_special_result <= 32'h80000000;
            else s2_special_result <= {s1_in1_init.sign ^ s1_in2_init.sign,31'd0};
        //else
        end else begin
            s2_special_case <= 0;
            //this doesnt matter, but its there to show intent that the special result doesnt get propagated since special case is zero
            s2_special_result <= '0;
        end
    end
end

logic[8:0] s2_exponent_add; //extra bit to account for overflow
assign s2_exponent_add = s2_in1.exponent + s2_in2.exponent + 8'd127;

logic s2_sign_bit;
assign s2_sign_bit = s2_in1.sign ^ s2_in2.sign;

logic[23:0] s2_multiplier_in1, s2_multiplier_in2;
assign s2_multiplier_in1 = {1'b1, s2_in1.mantissa};
assign s2_multiplier_in2 = {1'b1, s2_in2.mantissa};

fp_32b_t s3_special_result;
logic s3_input_is_invalid;
logic s3_input_is_flushed;
logic s3_special_case;
logic[2:0] s3_rounding_mode;

logic[8:0] s3_exponent_add;
logic s3_sign_bit;



fp_32b_t s4_special_result;
logic s4_input_is_invalid;
logic s4_input_is_flushed;
logic s4_special_case;
logic[2:0] s4_rounding_mode;

logic s4_sign_bit;
logic[8:0] s4_exponent_add;
logic[47:0] s4_multiplier_out;
logic s4_valid_data_in;

Dadda_Multiplier_24bit_pipelined s2_4_multiplier(.clk(clk), .rst(rst), .valid_data_in(s2_valid_data_in), 
.in1(s2_multiplier_in1), .in2(s2_multiplier_in2), .out(s4_multiplier_out), .valid_data_out(s4_valid_data_in));

endmodule