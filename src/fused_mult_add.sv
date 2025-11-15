import fp_pkg::*;

//(in1 * in2) + in3
module fp_fused_mult_add_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2, in3,
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
        end else if(s1_in1_iszero | s1_in2_iszero) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in3_init;
        end else begin
            s2_special_case <= 0;
            //this doesnt matter, but its there to show intent that the special result doesnt get propagated since special case is zero
            s2_special_result <= '0;
        end
    end
end
logic signed[9:0] s2_mult_exponent_add; //extra bit to account for overflow
assign s2_mult_exponent_add = $signed({1'b0, s2_in1.exponent}) + $signed({1'b0, s2_in2.exponent}) - 10'sd127;

logic s2_mult_sign_bit;
assign s2_mult_sign_bit = s2_in1.sign ^ s2_in2.sign;

logic[23:0] s2_multiplier_in1, s2_multiplier_in2;
assign s2_multiplier_in1 = {1'b1, s2_in1.mantissa};
assign s2_multiplier_in2 = {1'b1, s2_in2.mantissa};


fp_32b_t s3_special_result, s3_in3;
logic s3_valid_data_in;
logic s3_input_is_invalid;
logic s3_input_is_flushed;
logic s3_special_case;
logic[2:0] s3_rounding_mode;

logic signed[9:0] s3_mult_exponent_add;
logic s3_mult_sign_bit;

fp_32b_t s4_special_result, s4_in3;
logic s4_valid_data_in;
logic s4_input_is_invalid;
logic s4_input_is_flushed;
logic s4_special_case;
logic[2:0] s4_rounding_mode;

logic s4_mult_sign_bit;
logic signed[9:0] s4_mult_exponent_add;
logic[47:0] s4_multiplier_out;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_special_result <= 0;
        s3_input_is_invalid <= 0;
        s3_input_is_flushed <= 0;
        s3_special_case <= 0;
        s3_rounding_mode <= 0;
        s3_valid_data_in <= 0;
        s3_mult_exponent_add <= 0;
        s3_mult_sign_bit <= 0;
        s3_in3 <= 0;

        s4_special_result <= 0;
        s4_input_is_invalid <= 0;
        s4_input_is_flushed <= 0;
        s4_special_case <= 0;
        s4_rounding_mode <= 0;
        s4_valid_data_in <= 0;
        s4_mult_sign_bit <= 0;
        s4_mult_exponent_add <= 0;
        s4_in3 <= 0;
    end else begin
        s3_special_result <= s2_special_result;
        s3_input_is_invalid <= s2_input_is_invalid;
        s3_input_is_flushed <= s2_input_is_flushed;
        s3_special_case <= s2_special_case;
        s3_rounding_mode <= s2_rounding_mode;
        s3_valid_data_in <= s2_valid_data_in;
        s3_mult_exponent_add <= s2_mult_exponent_add;
        s3_mult_sign_bit <= s2_mult_sign_bit;
        s3_in3 <= s2_in3;

        s4_special_result <= s3_special_result;
        s4_input_is_invalid <= s3_input_is_invalid;
        s4_input_is_flushed <= s3_input_is_flushed;
        s4_special_case <= s3_special_case;
        s4_rounding_mode <= s3_rounding_mode;
        s4_valid_data_in <= s3_valid_data_in;
        s4_mult_exponent_add <= s3_mult_exponent_add;
        s4_mult_sign_bit <= s3_mult_sign_bit;
        s4_in3 <= s3_in3;
    end
end

Dadda_Multiplier_24bit_pipelined s2_s4_multiplier(.clk(clk), .rst(rst), .in1(s2_multiplier_in1), .in2(s2_multiplier_in2), .out(s4_multiplier_out));

//output is Q2.46

//truncate to Q1.26
logic[26:0] s4_mult_out_truncated;
logic signed[9:0] s4_mult_normalized_exponent;
always_comb begin
    if(s4_multiplier_out[47]) begin
        s4_mult_out_truncated = s4_multiplier_out[46:20];
        s4_mult_normalized_exponent = s4_mult_exponent_add + 10'sd1;
    end else begin
        s4_mult_out_truncated = s4_multiplier_out[45:19];
        s4_mult_normalized_exponent = s4_mult_exponent_add;
    end
end


//determine the larger input, calculate shift amount, and shift smaller input
logic s4_mult_out_is_larger;
logic[9:0] s4_shift_amount; 
logic s4_op_is_subtraction;
logic[26:0] s4_larger_mantissa, s4_smaller_mantissa;
logic s4_larger_number_sign;
logic signed[9:0] s4_larger_number_exponent;

logic[53:0] s4_aligned_smaller_mantissa;
logic s4_alignment_sticky_bit;
logic[26:0] s4_in3_mantissa_extended;
logic signed[9:0] s4_in3_exponent_extended;
always_comb begin
    s4_op_is_subtraction = s4_mult_sign_bit ^ s4_in3.sign;

    s4_in3_mantissa_extended = {1'b1, s4_in3.mantissa, 3'b000};
    s4_in3_exponent_extended = {2'b00, s4_in3.exponent};
    s4_mult_out_is_larger = (s4_mult_normalized_exponent > s4_in3_exponent_extended) |
    ((s4_mult_out_truncated >= s4_in3_mantissa_extended) & (s4_mult_normalized_exponent == s4_in3_exponent_extended));

    if(s4_mult_out_is_larger) s4_shift_amount = s4_mult_normalized_exponent - s4_in3_exponent_extended;
    else s4_shift_amount = s4_in3_exponent_extended - s4_mult_normalized_exponent;

    if(s4_mult_out_is_larger) begin
        s4_larger_mantissa = s4_mult_out_truncated;
        s4_smaller_mantissa = s4_in3_mantissa_extended;
        s4_larger_number_exponent = s4_mult_normalized_exponent;
        s4_larger_number_sign = s4_mult_sign_bit;
    end
    else begin
        s4_larger_mantissa = s4_in3_mantissa_extended;
        s4_smaller_mantissa = s4_mult_out_truncated;
        s4_larger_number_exponent = s4_in3_exponent_extended;
        s4_larger_number_sign = s4_in3.sign;
    end

    s4_aligned_smaller_mantissa = s4_smaller_mantissa >> s4_shift_amount;

    if(s4_shift_amount >= 54) begin
        s4_alignment_sticky_bit = 1;
    end else begin
        s4_alignment_sticky_bit = | s4_aligned_smaller_mantissa[26:0];
    end
end

//store stage 4 values
fp_32b_t s5_special_result;
logic s5_input_is_invalid;
logic s5_input_is_flushed;
logic s5_special_case;
logic s5_valid_data_in;
logic[2:0] s5_rounding_mode;

logic[26:0] s5_aligned_smaller_mantissa;
logic s5_alignment_sticky_bit;
logic signed[9:0] s5_larger_number_exponent;
logic[26:0] s5_larger_mantissa;
logic s5_larger_sign;
logic s5_op_is_subtraction;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s5_special_result <= '0;
        s5_input_is_invalid <= 0;
        s5_input_is_flushed <= 0;
        s5_special_case <= 0;
        s5_valid_data_in <= 0;
        s5_rounding_mode <= 0;
        s5_aligned_smaller_mantissa <= '0;
        s5_alignment_sticky_bit <= 0;
        s5_larger_number_exponent <= '0;
        s5_larger_mantissa <= '0;
        s5_larger_sign <= 0;
        s5_op_is_subtraction <= 0;
    end
    else begin
        s5_special_result <= s4_special_result;
        s5_input_is_invalid <= s4_input_is_invalid;
        s5_input_is_flushed <= s4_input_is_flushed;
        s5_special_case <= s4_special_case;
        s5_valid_data_in <= s4_valid_data_in;
        s5_rounding_mode <= s4_rounding_mode;
        s5_aligned_smaller_mantissa <= s4_aligned_smaller_mantissa[53:27]; 
        s5_alignment_sticky_bit <= s4_alignment_sticky_bit;
        s5_larger_number_exponent <= s4_larger_number_exponent;
        s5_larger_mantissa <= s4_larger_mantissa;
        s5_larger_sign <= s4_larger_number_sign;
        s5_op_is_subtraction <= s4_op_is_subtraction;
    end
end

//stage 5: add/subtract
logic[26:0] s5_addition_result_sum;
logic s5_addition_result_carry;
logic[26:0] s5_subtraction_result;
logic s5_exact_zero;
logic[26:0] s5_subtractor_input2;

KSA_nbits #(.WIDTH(27)) s5_adder (.in1(s5_larger_mantissa), .in2(s5_aligned_smaller_mantissa), .out(s5_addition_result_sum), .cout(s5_addition_result_carry));

assign s5_subtractor_input2 = ~s5_aligned_smaller_mantissa + 1'b1;

KSA_nbits #(.WIDTH(27)) s3_subtractor (.in1(s5_larger_mantissa), .in2(s5_subtractor_input2), .out(s5_subtraction_result));

assign s5_exact_zero = s5_op_is_subtraction ? (s5_larger_mantissa == s5_aligned_smaller_mantissa) : 1'b0;

//store stage 5 values
fp_32b_t s6_special_result;
logic s6_input_is_invalid;
logic s6_input_is_flushed;
logic s6_special_case;
logic s6_valid_data_in;
logic[2:0] s6_rounding_mode;

logic[27:0] s6_addition_result;
logic[26:0] s6_subtraction_result;
logic s6_op_is_subtraction;
logic s6_alignment_sticky_bit;
logic s6_exact_zero;
logic signed[9:0] s6_larger_number_exponent;
logic s6_larger_number_sign;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s6_special_result <= '0;
        s6_input_is_invalid <= 0;
        s6_input_is_flushed <= 0;
        s6_special_case <= 0;
        s6_valid_data_in <= 0;
        s6_rounding_mode <= 0;

        s6_addition_result <= '0;
        s6_subtraction_result <= '0;
        s6_op_is_subtraction <= 0;
        s6_alignment_sticky_bit <= 0;
        s6_exact_zero <= 0;
        s6_larger_number_exponent <= '0;
        s6_larger_number_sign <= 0;
    end
    else begin
        s6_special_result <= s5_special_result;
        s6_input_is_invalid <= s5_input_is_invalid;
        s6_input_is_flushed <= s5_input_is_flushed;
        s6_special_case <= s5_special_case;
        s6_valid_data_in <= s5_valid_data_in;
        s6_rounding_mode <= s5_rounding_mode;

        s6_addition_result <= {s5_addition_result_carry, s5_addition_result_sum};
        s6_subtraction_result <= s5_subtraction_result;
        s6_op_is_subtraction <= s5_op_is_subtraction;
        s6_alignment_sticky_bit <= s5_alignment_sticky_bit;
        s6_exact_zero <= s5_exact_zero;
        s6_larger_number_exponent <= s5_larger_number_exponent;
        s6_larger_number_sign <= s5_larger_sign;
    end
end

//Stage 6: Normalization and Rounding

//determine is adding caused an overflow, if so left shift by one and add one to the exponent 
logic s6_add_overflow;
logic[22:0] s6_add_normalized_mantissa;
logic signed[9:0] s6_add_normalized_exponent;
logic s6_add_normalized_guard, s6_add_normalized_round, s6_add_normalized_sticky;
always_comb begin
    s6_add_overflow = s6_addition_result[27];
    //addition overflow
    if(s6_add_overflow) begin
        s6_add_normalized_mantissa  = s6_addition_result[26:4];
        s6_add_normalized_guard = s6_addition_result[3];
        s6_add_normalized_round = s6_addition_result[2];
        s6_add_normalized_sticky = s6_addition_result[1] | s6_addition_result[0] | s6_alignment_sticky_bit;
    end
    else begin
        s6_add_normalized_mantissa  = s6_addition_result[25:3];
        s6_add_normalized_guard = s6_addition_result[2];
        s6_add_normalized_round = s6_addition_result[1];
        s6_add_normalized_sticky = s6_addition_result[0] | s6_alignment_sticky_bit;
    end
    //extra bit is added to detect overflow
    s6_add_normalized_exponent = s6_larger_number_exponent + s6_add_overflow;
end

//subtraction normalization
logic[4:0] s6_sub_shift_amount;
leading_zero_detection leading_zero_detector(.sub_result(s6_subtraction_result), .shift_amount(s6_sub_shift_amount));
logic[25:0] s6_sub_normalized_mantissa_temp;
logic[22:0] s6_sub_normalized_mantissa;
logic signed[9:0] s6_sub_normalized_exponent;
logic s6_sub_normalized_guard, s6_sub_normalized_round, s6_sub_normalized_sticky;
always_comb begin
    s6_sub_normalized_mantissa_temp = s6_subtraction_result[25:0] << s6_sub_shift_amount;
    //extra bit is added to detect underflow
    s6_sub_normalized_exponent = s6_larger_number_exponent - {5'b00000, s6_sub_shift_amount};

    s6_sub_normalized_mantissa = s6_sub_normalized_mantissa_temp[25:3];
    s6_sub_normalized_guard = s6_sub_normalized_mantissa_temp[2];
    s6_sub_normalized_round = s6_sub_normalized_mantissa_temp[1];
    s6_sub_normalized_sticky = s6_sub_normalized_mantissa_temp[0] | s6_alignment_sticky_bit;
end

//forward relevant result
logic[22:0] s6_normalized_mantissa;
logic[8:0] s6_normalized_exponent;
logic s6_normalized_guard, s6_normalized_round, s6_normalized_sticky;
always_comb begin
    if(s6_op_is_subtraction) begin
        s6_normalized_mantissa = s6_sub_normalized_mantissa;
        s6_normalized_exponent = s6_sub_normalized_exponent;
        s6_normalized_guard = s6_sub_normalized_guard;
        s6_normalized_round = s6_sub_normalized_round;
        s6_normalized_sticky = s6_sub_normalized_sticky;
    end else begin
        s6_normalized_mantissa = s6_add_normalized_mantissa;
        s6_normalized_exponent = s6_add_normalized_exponent;
        s6_normalized_guard = s6_add_normalized_guard;
        s6_normalized_round = s6_add_normalized_round;
        s6_normalized_sticky = s6_add_normalized_sticky;
    end
end

//rounding and flush to zero 
logic[23:0] s6_rounded_mantissa_temp;
logic[22:0] s6_rounded_mantissa;
logic signed [9:0] s6_rounded_exponent;
logic s6_exponent_overflow, s6_exponent_underflow, s6_has_grs_bits;
floating_point_rounder rounder(.mantissa(s6_normalized_mantissa), .guard(s6_normalized_guard), .round(s6_normalized_round), .sticky(s6_normalized_sticky),
.sign(s6_larger_number_sign), .rounding_mode(s6_rounding_mode), .rounded_mantissa_pre_overflow_detection(s6_rounded_mantissa_temp));
always_comb begin
    if(s6_rounded_mantissa_temp[23]) begin
        s6_rounded_mantissa = '0;
        s6_rounded_exponent = s6_normalized_exponent + 1;
    end else begin
        s6_rounded_mantissa = s6_rounded_mantissa_temp[22:0];
        s6_rounded_exponent = s6_normalized_exponent;
    end

    s6_exponent_overflow = (s6_normalized_exponent > 10'sd254) & !s6_op_is_subtraction;
    s6_exponent_underflow = ((s6_rounded_exponent <= 0)) & s6_op_is_subtraction;
    s6_has_grs_bits = s6_normalized_guard | s6_normalized_round | s6_normalized_sticky;

end
//propagate final values to output
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin   
        out <= '0;
        overflow <= 0;
        underflow <= 0;
        inexact <= 0;
        invalid_operation <= 0;
        valid_data_out <= 0;

    end else begin
        invalid_operation <= s6_input_is_invalid;
        valid_data_out <= s6_valid_data_in;
        if(s6_special_case) begin
            overflow <= 1'b0;
            inexact <= 1'b0;
            underflow <= s6_input_is_flushed;
            out <= s6_special_result;
        end else if(s6_exact_zero) begin
            overflow <= 1'b0;
            underflow <= s6_input_is_flushed;
            inexact <= 1'b0;
            if(s6_rounding_mode == RDN) out <= {1'b1, 31'b0};
            else out <= 32'b0;
        end else if(s6_exponent_overflow) begin
            overflow <= 1'b1;
            underflow <= 1'b0;
            inexact <= 1'b1;
            case(s6_rounding_mode)
                RTZ: begin
                    out <= {s6_larger_number_sign, 8'hFE, 23'h7FFFFF};
                end
                RDN: begin
                    if(s6_larger_number_sign) begin
                        out <= {1'b1, 8'hFF, 23'h0};
                    end else begin
                        out <= {1'b0, 8'hFE, 23'h7FFFFF};
                    end
                end
                RUP: begin
                    if(s6_larger_number_sign) begin
                        out <= {1'b1, 8'hFE, 23'h7FFFFF};
                    end else begin
                        out <= {1'b0, 8'hFF, 23'h0};
                    end
                end
                default: begin
                    out <= {s6_larger_number_sign, 8'hFF, 23'h0};
                end
            endcase
        end else if(s6_exponent_underflow) begin
            overflow <= 1'b0;
            underflow <= s6_has_grs_bits;
            inexact <= s6_has_grs_bits;
            if(s6_rounding_mode == RDN) begin
                out <= {1'b1, 8'h0, 23'h0};
            end else begin
                out <= {s6_larger_number_sign, 8'h0, 23'h0};
            end
        end else begin
            overflow <= 1'b0;
            underflow <= 1'b0;
            inexact <= s6_has_grs_bits;
            out <= {s6_larger_number_sign, s6_rounded_exponent[7:0], s6_rounded_mantissa};
        end
    end
end

endmodule