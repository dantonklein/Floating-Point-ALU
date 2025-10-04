//theres some sort of bug with the rounding idek what it is but it makes the answer 1 wrong sometimes

import fp_pkg::*;

module fp_addsub_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation,
    output logic valid_data_out,
    output logic normalized_mantissa_lsb, normalized_guard, normalized_round, normalized_sticky, round_up
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
        //both positive zero -> positive zero or both negative zero -> negative zero
            if(s1_in1_init.sign == s1_in2_init.sign) begin
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
            //if(s1_in2_isdenorm) s2_special_result <= {s1_in2_init.sign, 31'd0};
            //else 
            s2_special_result <= s1_in2_init;

        end else if(s1_in2_iszero | s1_in2_isdenorm) begin
            s2_special_case <= 1;
            //if(s1_in1_isdenorm) s2_special_result <= {s1_in1_init.sign, 31'd0};
            //else 
            s2_special_result <= s1_in1_init;
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
fp_32b_t s2_larger;
logic [22:0] s2_smaller_mantissa;

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
        s2_smaller_mantissa = s2_in2.mantissa;
    end
    else begin
        s2_larger = s2_in2;
        s2_smaller_mantissa = s2_in1.mantissa;
    end
    //allign smaller number's exponent
    s2_aligned_smaller_mantissa = {1'b1, s2_smaller_mantissa,27'd0} >> s2_shift_amount;
    if(s2_shift_amount) s2_alignment_sticky_bit = | s2_aligned_smaller_mantissa[24:0];
    else s2_alignment_sticky_bit = 0;

    //s2_alignment_sticky_bit = | s2_aligned_smaller_mantissa[23:0];
    //s2_alignment_sticky_bit = (| s2_aligned_smaller_mantissa[23:0]) | (s2_shift_amount == 5'd24);
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
logic[7:0] s4_larger_number_exponent;
logic s4_larger_number_sign;

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
        s4_larger_number_exponent <= '0;
        s4_larger_number_sign <= 0;
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
        s4_larger_number_exponent <= s3_larger_number.exponent;
        s4_larger_number_sign <= s3_larger_number.sign;
    end
end

//Stage 4: Normalization and Rounding

//determine is adding caused an overflow, if so left shift by one and add one to the exponent 
logic s4_add_overflow;
logic[22:0] s4_add_normalized_mantissa;
logic[8:0] s4_add_normalized_exponent;
logic s4_add_normalized_guard, s4_add_normalized_round, s4_add_normalized_sticky;
always_comb begin
    s4_add_overflow = s4_addition_result[27];
    //addition overflow
    if(s4_add_overflow) begin
        s4_add_normalized_mantissa  = s4_addition_result[26:4];
        s4_add_normalized_guard = s4_addition_result[3];
        s4_add_normalized_round = s4_addition_result[2];
        s4_add_normalized_sticky = s4_addition_result[1] | s4_addition_result[0] | s4_alignment_sticky_bit;
    end
    else begin
        s4_add_normalized_mantissa  = s4_addition_result[25:3];
        s4_add_normalized_guard = s4_addition_result[2];
        s4_add_normalized_round = s4_addition_result[1];
        s4_add_normalized_sticky = s4_addition_result[0] | s4_alignment_sticky_bit;
    end
    //extra bit is added to detect overflow
    s4_add_normalized_exponent = {1'b0,s4_larger_number_exponent} + s4_add_overflow;
end

//subtraction normalization
logic[4:0] s4_sub_shift_amount;
leading_zero_detection leading_zero_detector(.sub_result(s4_subtraction_result), .shift_amount(s4_sub_shift_amount));
logic[25:0] s4_sub_normalized_mantissa_temp;
logic[22:0] s4_sub_normalized_mantissa;
logic[8:0] s4_sub_normalized_exponent;
logic s4_sub_normalized_guard, s4_sub_normalized_round, s4_sub_normalized_sticky;
always_comb begin
    s4_sub_normalized_mantissa_temp = s4_subtraction_result[25:0] << s4_sub_shift_amount;
    //extra bit is added to detect underflow
    s4_sub_normalized_exponent = {1'b0, s4_larger_number_exponent} - {4'b0000, s4_sub_shift_amount};

    s4_sub_normalized_mantissa = s4_sub_normalized_mantissa_temp[25:3];
    s4_sub_normalized_guard = s4_sub_normalized_mantissa_temp[2];
    s4_sub_normalized_round = s4_sub_normalized_mantissa_temp[1];
    s4_sub_normalized_sticky = s4_sub_normalized_mantissa_temp[0] | s4_alignment_sticky_bit;
end

//forward relevant result
logic[22:0] s4_normalized_mantissa;
logic[7:0] s4_normalized_exponent;
logic s4_normalized_overflow_underflow;
logic s4_normalized_guard, s4_normalized_round, s4_normalized_sticky;
always_comb begin
    if(s4_op_is_subtraction) begin
        s4_normalized_mantissa = s4_sub_normalized_mantissa;
        s4_normalized_exponent = s4_sub_normalized_exponent[7:0];
        s4_normalized_overflow_underflow = s4_sub_normalized_exponent[8];
        s4_normalized_guard = s4_sub_normalized_guard;
        s4_normalized_round = s4_sub_normalized_round;
        s4_normalized_sticky = s4_sub_normalized_sticky;
    end else begin
        s4_normalized_mantissa = s4_add_normalized_mantissa;
        s4_normalized_exponent = s4_add_normalized_exponent[7:0];
        s4_normalized_overflow_underflow = s4_add_normalized_exponent[8];
        s4_normalized_guard = s4_add_normalized_guard;
        s4_normalized_round = s4_add_normalized_round;
        s4_normalized_sticky = s4_add_normalized_sticky;
    end
end

//rounding and flush to zero 
fp_32b_t s4_rounded_output;
logic[23:0] s4_rounded_mantissa_temp;
logic s4_exponent_overflow, s4_exponent_underflow, s4_has_grs_bits;
logic s4_overflow_exception, s4_inexact_exception, s4_underflow_exception;
logic s4_round_up;

always_comb begin
    s4_exponent_overflow = s4_normalized_overflow_underflow & !s4_op_is_subtraction;
    s4_exponent_underflow = s4_normalized_overflow_underflow & s4_op_is_subtraction;
    s4_has_grs_bits = s4_normalized_guard | s4_normalized_round | s4_normalized_sticky;

    //rounding logic

    //default logic
    s4_overflow_exception = 0;
    s4_underflow_exception = 0;
    s4_round_up = 1'b0;
    if(s4_exponent_underflow) begin
        s4_rounded_output.exponent = '0;
        s4_rounded_output.mantissa = '0;
        s4_underflow_exception = s4_has_grs_bits;

    end else if (s4_exponent_overflow) begin
        s4_overflow_exception = 1'b1;
        case(s4_rounding_mode) 
            RNE, RMM: begin
                s4_rounded_output.mantissa = '0;
                s4_rounded_output.exponent = '1;
            end
            RTZ: begin
                s4_rounded_output.mantissa = '1;
                s4_rounded_output.exponent = 8'b11111110;
            end
            RDN: begin
                if(s4_larger_number_sign) begin
                    s4_rounded_output.mantissa = '0;
                    s4_rounded_output.exponent = '1;
                end else begin
                    s4_rounded_output.mantissa = '1;
                    s4_rounded_output.exponent = 8'b11111110;
                end
            end
            RUP: begin
                if(s4_larger_number_sign) begin
                    s4_rounded_output.mantissa = '1;
                    s4_rounded_output.exponent = 8'b11111110;
                end else begin
                    s4_rounded_output.mantissa = '0;
                    s4_rounded_output.exponent = '1;
                end
            end
            default: begin
                s4_rounded_output.mantissa = '0;
                s4_rounded_output.exponent = '1;
            end
        endcase
    end else begin
        case(s4_rounding_mode)
            RNE: begin
                // s4_round_up = (s4_normalized_guard & (s4_normalized_round | s4_normalized_sticky)) | 
                //(s4_normalized_guard & ~s4_normalized_round & ~s4_normalized_sticky & s4_normalized_mantissa[0]);

                s4_round_up = s4_normalized_guard & (s4_normalized_round | s4_normalized_sticky | s4_normalized_mantissa[0]);
            end
            RTZ: begin
                s4_round_up = 1'b0;
            end
            RDN: begin
                s4_round_up = s4_larger_number_sign & (s4_normalized_guard | s4_normalized_round | s4_normalized_sticky);
            end
            RUP: begin
                s4_round_up = ~s4_larger_number_sign & (s4_normalized_guard | s4_normalized_round | s4_normalized_sticky);
            end
            RMM: begin
                s4_round_up = s4_normalized_guard;
            end
            default: begin
                s4_round_up = 1'b0;
            end
        endcase
    end

    s4_rounded_mantissa_temp = {1'b0, s4_normalized_mantissa} + s4_round_up;

    if(s4_rounded_mantissa_temp[23]) begin
        s4_rounded_output.mantissa = '0;
        if(s4_normalized_exponent == 8'd254) begin
            s4_rounded_output.exponent = '1;
            s4_overflow_exception = 1'b1;
        end else begin
            s4_rounded_output.exponent = s4_normalized_exponent + 1;
        end
    end else begin
        s4_rounded_output.exponent = s4_normalized_exponent;
        s4_rounded_output.mantissa = s4_rounded_mantissa_temp[22:0];
    end
    s4_rounded_output.sign = s4_larger_number_sign;
    s4_inexact_exception = s4_has_grs_bits;
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
        normalized_mantissa_lsb <= 0;
        normalized_guard <= 0;
        normalized_round <= 0;
        normalized_sticky <= 0;
        round_up <= 0;
    end else begin
        invalid_operation <= s4_input_is_invalid;
        valid_data_out <= s4_valid_data_in;
        normalized_mantissa_lsb <= s4_normalized_mantissa[0];
        normalized_guard <= s4_normalized_guard;
        normalized_round <= s4_normalized_round;
        normalized_sticky <= s4_normalized_sticky;
        round_up <= s4_round_up;
        if(s4_special_case) begin
            overflow <= 1'b0;
            underflow <= s4_input_is_flushed;
            inexact <= 1'b0;
            out <= s4_special_result;
        end else if(s4_exact_zero) begin
            overflow <= 1'b0;
            underflow <= s4_input_is_flushed;
            inexact <= 1'b0;
            if(s4_rounding_mode == RDN) out <= {1'b1, 31'b0};
            else out <= 32'b0;
        end else begin
            overflow <= s4_overflow_exception;
            underflow <= s4_input_is_flushed | s4_underflow_exception;
            inexact <= s4_inexact_exception;
            out <= s4_rounded_output;
        end
    end
end

endmodule