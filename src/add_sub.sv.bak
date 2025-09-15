import fp_pkg::*;

module fp_addsub_pipeline (
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
    s1_s1_in2_issnan = (s1_in2_init.exponent == '1) & (s1_in2_init.mantissa != '0) & ~s1_in2_init.mantissa[22];
    s1_in2_isdenorm = (s1_in2_init.exponent == '0) & (s1_in2_init.mantissa != '0);
end
//flush to zero and invalid input handling
logic s1_input_is_invalid;
logic s1_input_is_flushed;
assign s1_input_is_invalid = s1_in1_issnan | s1_in2_issnan | (s1_in1_isinfinite & s1_in2_isinfinite & (s1_in1_isinfinite ^ s1_in2_isinfinite));
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
        s2_rounding_mode <= 0;
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
        end else if(s1_in1_isinfinite & s1_in2_isinfinite) begin
            s2_special_case <= 1;
            //same sign means that infinity gets propagated
            if(s1_in1_init.sign == s1_in2_init.sign) begin
                s2_special_result <= s1_in1_init;
            //infinity - infinity = snan, converted to qnan but triggers the invalid operation
            end else begin
                s2_special_result <= 32'h7FC00000;
            end

        //infinite input + non-infinite, infinity gets propagated
        end else if(s1_in1_isinfinite) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in1_init;
        end else if(s1_in2_isinfinite) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in2_init;

        //if both zeroes, different cases
        end else if((s1_in1_iszero | s1_in1_isdenorm) & (s1_in2_iszero | s1_in2_isdenorm)) begin
            s2_special_case <= 1;
        //both positive zero -> positive zero
            if(s1_in1_init.sign & s1_in2_init.sign) begin
                s2_special_result <= s1_in1_init;
        //both negative zero -> negative zero
            end else if(~s1_in1_init.sign & ~s1_in2_init.sign) begin
                s2_special_result <= s1_in1_init;
        //round down(RD) is a special case for when the signs differ
            end else begin
                if(rounding_mode == RDN) begin
                    s2_special_result <= 32'h80000000;
                end else begin
                    s2_special_result <= 32'h00000000;
                end
            end
        //any input is zero or denormal, propagate the other 
        end else if(s1_in1_iszero | s1_in1_isdenorm) begin
            s2_special_case <= 1;
            if(s1_in2_isdenorm) s2_special_result <= {s1_in2_init.sign, 31'd0};
            else s2_special_result <= s1_in2_init;

        end else if(s1_in2_iszero | s1_in2_isdenorm) begin
            s2_special_case <= 1;
            if(s1_in1_isdenorm) s2_special_result <= {s1_in1_init.sign, 31'd0};
            else s2_special_result <= s1_in1_init;
        end else begin
            s2_special_case <= 0;
            //this doesnt matter, but its there to show intent that the special result doesnt get propagated since special case is zero
            s2_special_result <= '0;
        end
    end
end

//Stage 2: Determine larger input, calculate shifting for smaller input, and determine if the operation is addition or subtraction


//determine the larger input, calculate shift amount, and shift smaller input
logic s2_in1_is_larger;
logic[7:0] s2_shift_amount_temp; //temp value before it gets cut off at 24
logic[4:0] s2_shift_amount;
logic s2_op_is_subtraction;
fp_32b_t s2_larger, s2_smaller;

logic[50:0] s2_aligned_smaller_mantissa;
logic s2_alignment_sticky_bit;
always_comb begin
    s2_op_is_subtraction = s2_in1.sign ^ s2_in2.sign;

    s2_in1_is_larger = 
    (s2_in1.exponent > s2_in2.exponent) | 
    ((s2_in1.exponent == s2_in2.exponent) & ({1'b1, s2_in1.mantissa} >= {1'b1, s2_in2.mantissa}));

    if(s2_in1_is_larger) s2_shift_amount_temp = s2_in1.exponent - s2_in2.exponent;
    else s2_shift_amount_temp = s2_in2.exponent - s2_in1.exponent;

    if(s2_shift_amount_temp > 24) s2_shift_amount = 5'd24;
    else s2_shift_amount = s2_shift_amount_temp[4:0];

    if(s2_in1_is_larger) begin
        s2_larger = s2_in1;
        s2_smaller = s2_in2;
    end
    else begin
        s2_larger = s2_in2;
        s2_smaller = s2_in1;
    end
    //allign smaller number's exponent
    s2_aligned_smaller_mantissa = {1'b1, s2_smaller.mantissa,3'b000} >> s2_shift_amount;
    if(s2_shift_amount) s2_alignment_sticky_bit = | s2_aligned_smaller_mantissa[24:0];
    else s2_alignment_sticky_bit = 0;
end

//store stage 2 values
fp_32b_t s3_special_result;
logic s3_input_is_invalid;
logic s3_input_is_flushed;
logic s3_special_case;
logic s3_valid_data_in;
logic[2:0] s3_rounding_mode;

logic[26:0] s3_aligned_smaller_mantissa;
logic s3_alignment_sticky_bit;
fp_32b_t s3_larger_number;
logic s3_op_is_subtraction;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_special_result <= '0;
        s3_input_is_invalid <= 0;
        s3_input_is_flushed <= 0;
        s3_special_case <= 0;
        s3_valid_data_in <= 0;
        s3_rounding_mode <= 0;
        s3_aligned_smaller_mantissa <= '0;
        s3_alignment_sticky_bit <= 0;
        s3_larger_number <= '0;
        s3_op_is_subtraction <= 0;
    end
    else begin
        s3_special_result <= s2_special_result;
        s3_input_is_invalid <= s2_input_is_invalid;
        s3_input_is_flushed <= s2_input_is_flushed;
        s3_special_case <= s2_special_case;
        s3_valid_data_in <= s2_valid_data_in;
        s3_rounding_mode <= s2_rounding_mode;
        s3_aligned_smaller_mantissa <= s2_aligned_smaller_mantissa[50:24]; 
        s3_alignment_sticky_bit <= s2_alignment_sticky_bit;
        s3_larger_number <= s2_larger;
        s3_op_is_subtraction <= s2_op_is_subtraction;
    end
end
//Stage 3: Add/Subtract
logic[27:0] s3_addition_result;
logic[26:0] s3_subtraction_result;
logic s3_exact_zero;

assign s3_addition_result = {2'b01, s3_larger_number.mantissa, 3'b000} + {1'b0, s3_aligned_smaller_mantissa};
assign s3_subtraction_result = {1'b1, s3_larger_number.mantissa, 3'b000} - s3_aligned_smaller_mantissa;
assign s3_exact_zero = s3_op_is_subtraction ? ({1'b1, s3_larger_number.mantissa, 3'b000} == {s3_aligned_smaller_mantissa}) : 1'b0;

//store stage 3 values
fp_32b_t s4_special_result;
logic s4_input_is_invalid;
logic s4_input_is_flushed;
logic s4_special_case;
logic s4_valid_data_in;
logic[2:0] s4_rounding_mode;

logic[27:0] s4_addition_result;
logic[26:0] s4_subtraction_result;
logic s4_op_is_subtraction;
logic s4_alignment_sticky_bit;
logic s4_exact_zero;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s4_special_result <= '0;
        s4_input_is_invalid <= 0;
        s4_input_is_flushed <= 0;
        s4_special_case <= 0;
        s4_valid_data_in <= 0;
        s4_rounding_mode <= 0;

        s4_addition_result <= '0;
        s4_subtraction_result <= '0;
        s4_op_is_subtraction <= 0;
        s4_alignment_sticky_bit <= 0;
        s4_exact_zero <= 0;
    end
    else begin
        s4_special_result <= s3_special_result;
        s4_input_is_invalid <= s3_input_is_invalid;
        s4_input_is_flushed <= s3_input_is_flushed;
        s4_special_case <= s3_special_case;
        s4_valid_data_in <= s3_valid_data_in;
        s4_rounding_mode <= s3_rounding_mode;

        s4_addition_result <= s3_addition_result;
        s4_subtraction_result <= s3_subtraction_result;
        s4_op_is_subtraction <= s3_op_is_subtraction;
        s4_alignment_sticky_bit <= s3_alignment_sticky_bit;
        s4_exact_zero <= s3_exact_zero;
    end
end

//Stage 4: Normalization and Rounding


endmodule