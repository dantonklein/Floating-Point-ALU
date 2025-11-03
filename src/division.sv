//input1/input2
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
assign s1_input_is_invalid = s1_in1_issnan | s1_in2_issnan | ((s1_in1_iszero | s1_in1_isdenorm) & (s1_in2_iszero | s1_in2_isdenorm)) | (s1_in1_isinfinite & s1_in2_isinfinite);
assign s1_input_is_flushed = s1_in1_isdenorm | s1_in2_isdenorm;

fp_32b_t s2_special_result, s2_in1, s2_in2;
logic s2_input_is_invalid;
logic s2_input_is_flushed;
logic s2_special_case;
logic s2_valid_data_in;
logic s2_division_by_zero;
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
        s2_division_by_zero <= 0;
    end else begin
        s2_in1 <= s1_in1_init;
        s2_in2 <= s1_in2_init;
        s2_input_is_invalid <= s1_input_is_invalid;
        s2_input_is_flushed <= s1_input_is_flushed;
        s2_valid_data_in <= valid_data_in;
        s2_rounding_mode <= rounding_mode;
        s2_division_by_zero <= (s1_in2_iszero | s1_in2_isdenorm) & ~s1_input_is_invalid;
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

        //when 0/0
        end else if((s1_in1_iszero | s1_in1_isdenorm) & (s1_in2_iszero | s1_in2_isdenorm)) begin
            //propagate qnan, exception flag is raised 
            s2_special_case <= 1;
            s2_special_result <= 32'h7FC00000;

        //when infinity/infinity
        end else if(s1_in1_isinfinite & s1_in2_isinfinite) begin
            s2_special_case <= 1;
            s2_special_result <= 32'h7FC00000;

        //if input 1 is zero
        end else if(s1_in1_iszero | s1_in1_isdenorm) begin
            s2_special_case <= 1;
            if(rounding_mode == RDN) s2_special_result <= 32'h80000000;
            else s2_special_result <= {s1_in1_init.sign ^ s1_in2_init.sign,31'd0};

        //if input 2 is infinity
        end else if(s1_in2_isinfinite) begin
            s2_special_case <= 1;
            if(rounding_mode == RDN) s2_special_result <= 32'h80000000;
            else s2_special_result <= {s1_in1_init.sign ^ s1_in2_init.sign,31'd0};
        
        //if input 1 is infinity
        end else if(s1_in1_isinfinite) begin
            s2_special_case <= 1;
            s2_special_result <= {s1_in1_init.sign ^ s1_in2_init.sign,31'h7F800000};

        //else
        end else begin
            s2_special_case <= 0;
            //this doesnt matter, but its there to show intent that the special result doesnt get propagated since special case is zero
            s2_special_result <= '0;
        end
    end
end

logic signed[9:0] s2_exponent_sub; //extra bit to account for overflow
assign s2_exponent_sub = $signed({1'b0, s2_in1.exponent}) - $signed({1'b0, s2_in2.exponent}) + 10'sd127;

logic s2_sign_bit;
assign s2_sign_bit = s2_in1.sign ^ s2_in2.sign;


logic signed[9:0] s3_s13_new_exponents[11];
fp_32b_t s3_s13_in1[11];
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_s13_new_exponents[0] <= 0;
        s3_s13_in1[0] <= 0;
        for(int i = 0; i < 10; i++) begin
            s3_s13_new_exponents[i+1] <= 0;
            s3_s13_in1[i+1] <= 0;
        end
    end else begin
        s3_s13_new_exponents[0] <= s2_exponent_sub;
        s3_s13_in1[0] <= s2_in1;
        for(int i = 0; i < 10 i++) begin
            s3_s13_new_exponents[i+1] <= s3_s13_new_exponents[i];
        end
    end
end

//stage 13
//output is Q2.52
logic[53:0] s13_reciprocal_out;
logic[26:0] s13_reciprocal_out_truncated;
logic s13_valid_data_in;
logic[2:0] s13_rounding_mode;
fp_32b_t s13_special_result;
logic s13_input_is_invalid;
logic s13_input_is_flushed;
logic s13_special_case;
logic s13_sign;
//i stuffed all the signals that need to be propagated in the module since i already needed later versions of the rounding mode and sign
mantissa_reciprocal s13_reciprocal(.clk(clk), .rst(rst), .valid_data_in(s2_valid_data_in), .in(s2_in2.mantissa), .rounding_mode(s2_rounding_mode), .sign(s2_sign_bit),
.special_result(s2_special_result), .input_is_invalid(s2_input_is_invalid), .input_is_flushed(s2_input_is_flushed), .special_case(s2_special_case), .out(s13_reciprocal_out),
.valid_data_out(s13_valid_data_in), .rounding_mode_out(s13_rounding_mode), .special_result_out(s13_special_result), .input_is_invalid_out(s13_input_is_invalid), 
.input_is_flushed_out(s13_input_is_flushed), .special_case_out(s13_special_case), .sign_out(s13_sign));

//truncate to Q1.26
assign s13_reciprocal_out_truncated = s13_reciprocal_out[52:26];

fp_32b_t s14_special_result;
logic s14_valid_data_in;
logic s14_input_is_invalid;
logic s14_input_is_flushed;
logic s14_special_case;
logic[2:0] s14_rounding_mode;
logic s14_sign_bit;
logic signed[9:0] s14_exponent;

fp_32b_t s15_special_result;
logic s15_valid_data_in;
logic s15_input_is_invalid;
logic s15_input_is_flushed;
logic s15_special_case;
logic[2:0] s15_rounding_mode;
logic s15_sign_bit;
logic signed[9:0] s15_exponent;
logic[53:0] s15_mult_out;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s14_special_result <= 0;
        s14_valid_data_in <= 0;
        s14_input_is_invalid <= 0;
        s14_input_is_flushed <= 0;
        s14_special_case <= 0;
        s14_rounding_mode <= 0;
        s14_sign_bit <= 0;
        s14_exponent <= 0;
        
        s15_special_result <= 0;
        s15_valid_data_in <= 0;
        s15_input_is_invalid <= 0;
        s15_input_is_flushed <= 0;
        s15_special_case <= 0;
        s15_rounding_mode <= 0;
        s15_sign_bit <= 0;
        s15_exponent <= 0;
    end else begin
        s14_special_result <= s13_special_result;
        s14_valid_data_in <= s13_valid_data_in;
        s14_input_is_invalid <= s13_input_is_invalid;
        s14_input_is_flushed <= s13_input_is_flushed;
        s14_special_case <= s13_special_case;
        s14_rounding_mode <= s13_rounding_mode;
        s14_sign_bit <= s13_sign_bit;
        s14_exponent <= s13_exponent;
        
        s15_special_result <= s14_special_result;
        s15_valid_data_in <= s14_valid_data_in;
        s15_input_is_invalid <= s14_input_is_invalid;
        s15_input_is_flushed <= s14_input_is_flushed;
        s15_special_case <= s14_special_case;
        s15_rounding_mode <= s14_rounding_mode;
        s15_sign_bit <= s14_sign_bit;
        s15_exponent <= s14_exponent;
    end
end

multiplier_delayed #(.WIDTH(27)) s13_s15_mult(.clk(clk), .rst(rst), .in1(s3_s13_in1[10]), .in2(s13_reciprocal_out), .out(s15_mult_out));
endmodule