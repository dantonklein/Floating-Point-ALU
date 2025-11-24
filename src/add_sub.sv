//theres some sort of bug with the rounding idek what it is but it makes the answer 1 wrong sometimes

import fp_pkg::*;

module fp_addsub_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation,
    //output logic guard, round, sticky, 
    //output logic[22:0] normalized_mantissa,
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
assign s1_input_is_invalid = s1_in1_issnan | s1_in2_issnan | (s1_in1_isinfinite & s1_in2_isinfinite & (s1_in1_init.sign ^ s1_in2_init.sign));
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
            //s2_special_result <= {s1_in1_init.sign, 8'hFF, 1'b1, s1_in1_init.mantissa[21:0]};
            s2_special_result <= 32'h7FE00000;

        end else if(s1_in2_issnan) begin
            s2_special_case <= 1;
            //s2_special_result <= {s1_in2_init.sign, 8'hFF, 1'b1, s1_in2_init.mantissa[21:0]};
            s2_special_result <= 32'h7FE00000;
        //when both infinity
        end else if(s1_in1_isinfinite & s1_in2_isinfinite) begin
            s2_special_case <= 1;
            //same sign means that infinity gets propagated
            if(s1_in1_init.sign == s1_in2_init.sign) begin
                s2_special_result <= s1_in1_init;
            //infinity - infinity = snan, converted to qnan but triggers the invalid operation
            end else begin
                s2_special_result <= 32'hFFC00000;
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
logic[7:0] s2_shift_amount; 
logic s2_op_is_subtraction;
fp_32b_t s2_larger;
logic [22:0] s2_smaller_mantissa;

always_comb begin
    s2_op_is_subtraction = s2_in1.sign ^ s2_in2.sign;

    s2_in1_is_larger = 
    (s2_in1.exponent > s2_in2.exponent) | 
    ((s2_in1.exponent == s2_in2.exponent) & (s2_in1.mantissa >= s2_in2.mantissa));

    if(s2_in1_is_larger) s2_shift_amount = s2_in1.exponent - s2_in2.exponent;
    else s2_shift_amount = s2_in2.exponent - s2_in1.exponent;

end

//Stage 3: Alignment 2

fp_32b_t s3_special_result;
logic s3_input_is_invalid;
logic s3_input_is_flushed;
logic s3_special_case;
logic s3_valid_data_in;
logic[2:0] s3_rounding_mode;

fp_32b_t s3_in1, s3_in2;
fp_32b_t s3_larger;
logic s3_in1_is_larger;
logic[7:0] s3_shift_amount; 
logic [22:0] s3_smaller_mantissa;
logic[47:0] s3_aligned_smaller_mantissa;
logic s3_alignment_sticky_bit;
logic s3_op_is_subtraction;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_special_result <= '0;
        s3_input_is_invalid <= 0;
        s3_input_is_flushed <= 0;
        s3_special_case <= 0;
        s3_valid_data_in <= 0;
        s3_rounding_mode <= 0;
        s3_in1 <= 0;
        s3_in2 <= 0;
        s3_in1_is_larger <= 0;
        s3_shift_amount <= 0;
        s3_op_is_subtraction <= 0;
    end
    else begin
        s3_special_result <= s2_special_result;
        s3_input_is_invalid <= s2_input_is_invalid;
        s3_input_is_flushed <= s2_input_is_flushed;
        s3_special_case <= s2_special_case;
        s3_valid_data_in <= s2_valid_data_in;
        s3_rounding_mode <= s2_rounding_mode; 
        s3_in1 <= s2_in1;
        s3_in2 <= s2_in2;
        s3_in1_is_larger <= s2_in1_is_larger;
        s3_shift_amount <= s2_shift_amount;
        s3_op_is_subtraction <= s2_op_is_subtraction;
    end

end

always_comb begin
    if(s3_in1_is_larger) begin
        s3_larger = s3_in1;
        s3_smaller_mantissa = s3_in2.mantissa;
    end
    else begin
        s3_larger = s3_in2;
        s3_smaller_mantissa = s3_in1.mantissa;
    end
    s3_aligned_smaller_mantissa = {1'b1, s3_smaller_mantissa,24'd0} >> s3_shift_amount;

    if(s3_shift_amount >= 26) begin
        s3_alignment_sticky_bit = 1;
    end else begin
        s3_alignment_sticky_bit = | s3_aligned_smaller_mantissa[21:0];
    end
    
end




//store stage 3 values
fp_32b_t s4_special_result;
logic s4_input_is_invalid;
logic s4_input_is_flushed;
logic s4_special_case;
logic s4_valid_data_in;
logic[2:0] s4_rounding_mode;

logic[25:0] s4_aligned_smaller_mantissa;
logic s4_alignment_sticky_bit;
fp_32b_t s4_larger_number;
logic s4_op_is_subtraction;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s4_special_result <= '0;
        s4_input_is_invalid <= 0;
        s4_input_is_flushed <= 0;
        s4_special_case <= 0;
        s4_valid_data_in <= 0;
        s4_rounding_mode <= 0;
        s4_aligned_smaller_mantissa <= '0;
        s4_alignment_sticky_bit <= 0;
        s4_larger_number <= '0;
        s4_op_is_subtraction <= 0;
    end
    else begin
        s4_special_result <= s3_special_result;
        s4_input_is_invalid <= s3_input_is_invalid;
        s4_input_is_flushed <= s3_input_is_flushed;
        s4_special_case <= s3_special_case;
        s4_valid_data_in <= s3_valid_data_in;
        s4_rounding_mode <= s3_rounding_mode; 
        s4_aligned_smaller_mantissa <= s3_aligned_smaller_mantissa[47:22];
        s4_alignment_sticky_bit <= s3_alignment_sticky_bit;
        s4_larger_number <= s3_larger;
        s4_op_is_subtraction <= s3_op_is_subtraction;
    end
end
//Stage 4: Add/Subtract
logic[26:0] s4_addition_result_sum;
logic s4_addition_result_carry;
logic[26:0] s4_subtraction_result;
logic s4_exact_zero;
logic [26:0] s4_adder_input1, s4_adder_input2;
logic [26:0] s4_subtractor_input1, s4_subtractor_input2;
assign s4_adder_input1 = {1'b1, s4_larger_number.mantissa, 3'b000};
assign s4_adder_input2 = {s4_aligned_smaller_mantissa, s4_alignment_sticky_bit};

KSA_nbits #(.WIDTH(27)) s4_adder (.in1(s4_adder_input1), .in2(s4_adder_input2), .out(s4_addition_result_sum), .cout(s4_addition_result_carry));

assign s4_subtractor_input1 = {1'b1, s4_larger_number.mantissa, 3'b000};
assign s4_subtractor_input2 = ~{s4_aligned_smaller_mantissa, s4_alignment_sticky_bit} + 1'b1;

KSA_nbits #(.WIDTH(27)) s4_subtractor (.in1(s4_subtractor_input1), .in2(s4_subtractor_input2), .out(s4_subtraction_result), .cout());


assign s4_exact_zero = s4_op_is_subtraction ? ({1'b1, s4_larger_number.mantissa, 3'b000} == {s4_aligned_smaller_mantissa, 1'b0}) : 1'b0;

//store stage 3 values
fp_32b_t s5_special_result;
logic s5_input_is_invalid;
logic s5_input_is_flushed;
logic s5_special_case;
logic s5_valid_data_in;
logic[2:0] s5_rounding_mode;

logic[27:0] s5_addition_result;
logic[26:0] s5_subtraction_result;
logic s5_op_is_subtraction;
logic s5_exact_zero;
logic[7:0] s5_larger_number_exponent;
logic s5_larger_number_sign;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s5_special_result <= '0;
        s5_input_is_invalid <= 0;
        s5_input_is_flushed <= 0;
        s5_special_case <= 0;
        s5_valid_data_in <= 0;
        s5_rounding_mode <= 0;

        s5_addition_result <= '0;
        s5_subtraction_result <= '0;
        s5_op_is_subtraction <= 0;
        s5_exact_zero <= 0;
        s5_larger_number_exponent <= '0;
        s5_larger_number_sign <= 0;
    end
    else begin
        s5_special_result <= s4_special_result;
        s5_input_is_invalid <= s4_input_is_invalid;
        s5_input_is_flushed <= s4_input_is_flushed;
        s5_special_case <= s4_special_case;
        s5_valid_data_in <= s4_valid_data_in;
        s5_rounding_mode <= s4_rounding_mode;

        s5_addition_result <= {s4_addition_result_carry, s4_addition_result_sum};
        s5_subtraction_result <= s4_subtraction_result;
        s5_op_is_subtraction <= s4_op_is_subtraction;
        s5_exact_zero <= s4_exact_zero;
        s5_larger_number_exponent <= s4_larger_number.exponent;
        s5_larger_number_sign <= s4_larger_number.sign;
    end
end

//Stage 5: Normalization

//determine is adding caused an overflow, if so left shift by one and add one to the exponent 
logic s5_add_overflow;
logic[22:0] s5_add_normalized_mantissa;
logic[8:0] s5_add_normalized_exponent;
logic s5_add_normalized_guard, s5_add_normalized_round, s5_add_normalized_sticky;
always_comb begin
    s5_add_overflow = s5_addition_result[27];
    //addition overflow
    if(s5_add_overflow) begin
        s5_add_normalized_mantissa  = s5_addition_result[26:4];
        s5_add_normalized_guard = s5_addition_result[3];
        s5_add_normalized_round = s5_addition_result[2];
        s5_add_normalized_sticky = s5_addition_result[1] | s5_addition_result[0];
    end
    else begin
        s5_add_normalized_mantissa  = s5_addition_result[25:3];
        s5_add_normalized_guard = s5_addition_result[2];
        s5_add_normalized_round = s5_addition_result[1];
        s5_add_normalized_sticky = s5_addition_result[0];
    end
    //extra bit is added to detect overflow
    s5_add_normalized_exponent = {1'b0,s5_larger_number_exponent} + s5_add_overflow;
end

//subtraction normalization
logic[4:0] s5_sub_shift_amount;
leading_zero_detection leading_zero_detector(.sub_result(s5_subtraction_result), .shift_amount(s5_sub_shift_amount));
//logic[25:0] s5_sub_normalized_mantissa_temp;
logic[25:0] s5_sub_normalized_mantissa_temp;
logic[22:0] s5_sub_normalized_mantissa;
logic[8:0] s5_sub_normalized_exponent;
logic s5_sub_normalized_guard, s5_sub_normalized_round, s5_sub_normalized_sticky;
always_comb begin
    s5_sub_normalized_mantissa_temp = s5_subtraction_result[25:0] << s5_sub_shift_amount;
    //extra bit is added to detect underflow
    s5_sub_normalized_exponent = {1'b0, s5_larger_number_exponent} - {4'b0000, s5_sub_shift_amount};

    s5_sub_normalized_mantissa = s5_sub_normalized_mantissa_temp[25:3];
    s5_sub_normalized_guard = s5_sub_normalized_mantissa_temp[2];
    s5_sub_normalized_round = s5_sub_normalized_mantissa_temp[1];
    s5_sub_normalized_sticky = s5_sub_normalized_mantissa_temp[0];


end

//forward relevant result
logic[22:0] s5_normalized_mantissa;
logic[8:0] s5_normalized_exponent;
logic s5_normalized_guard, s5_normalized_round, s5_normalized_sticky;
always_comb begin
    if(s5_op_is_subtraction) begin
        s5_normalized_mantissa = s5_sub_normalized_mantissa;
        s5_normalized_exponent = s5_sub_normalized_exponent;
        s5_normalized_guard = s5_sub_normalized_guard;
        s5_normalized_round = s5_sub_normalized_round;
        s5_normalized_sticky = s5_sub_normalized_sticky;
    end else begin
        s5_normalized_mantissa = s5_add_normalized_mantissa;
        s5_normalized_exponent = s5_add_normalized_exponent;
        s5_normalized_guard = s5_add_normalized_guard;
        s5_normalized_round = s5_add_normalized_round;
        s5_normalized_sticky = s5_add_normalized_sticky;
    end
end
//Stage 6: rounding and output

logic[22:0] s6_normalized_mantissa;
logic[8:0] s6_normalized_exponent;
logic s6_normalized_guard;
logic s6_normalized_round;
logic s6_normalized_sticky;
logic s6_larger_number_sign;
logic s6_op_is_subtraction;
logic s6_exact_zero;


fp_32b_t s6_special_result;
logic s6_input_is_invalid;
logic s6_input_is_flushed;
logic s6_special_case;
logic s6_valid_data_in;
logic[2:0] s6_rounding_mode;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s6_normalized_mantissa <= 0;
        s6_normalized_exponent <= 0;
        s6_normalized_guard <= 0;
        s6_normalized_round <= 0;
        s6_normalized_sticky <= 0;
        s6_larger_number_sign <= 0;
        s6_op_is_subtraction <= 0;
        s6_exact_zero <= 0;

        s6_special_result <= 0;
        s6_input_is_invalid <= 0;
        s6_input_is_flushed <= 0;
        s6_special_case <= 0;
        s6_valid_data_in <= 0;
        s6_rounding_mode <= 0;
    end else begin
        s6_normalized_mantissa <= s5_normalized_mantissa;
        s6_normalized_exponent <= s5_normalized_exponent;
        s6_normalized_guard <= s5_normalized_guard;
        s6_normalized_round <= s5_normalized_round;
        s6_normalized_sticky <= s5_normalized_sticky;
        s6_larger_number_sign <= s5_larger_number_sign;
        s6_op_is_subtraction <= s5_op_is_subtraction;
        s6_exact_zero <= s5_exact_zero;

        s6_special_result <= s5_special_result;
        s6_input_is_invalid <= s5_input_is_invalid;
        s6_input_is_flushed <= s5_input_is_flushed;
        s6_special_case <= s5_special_case;
        s6_valid_data_in <= s5_valid_data_in;
        s6_rounding_mode <= s5_rounding_mode;
    end
end

//rounding and flush to zero 
logic[23:0] s6_rounded_mantissa_temp;
logic[22:0] s6_rounded_mantissa;
logic [8:0] s6_rounded_exponent;
logic s6_exponent_overflow, s6_exponent_underflow, s6_has_grs_bits;
floating_point_rounder rounder(.mantissa(s6_normalized_mantissa), .guard(s6_normalized_guard), .round(s6_normalized_round), .sticky(s6_normalized_sticky),
.sign(s6_larger_number_sign), .rounding_mode(s6_rounding_mode), .rounded_mantissa_pre_overflow_detection(s6_rounded_mantissa_temp));
always_comb begin
    if(s6_rounded_mantissa_temp[23]) begin
        s6_rounded_mantissa = '0;
        s6_rounded_exponent = s6_normalized_exponent + 9'd1;
    end else begin
        s6_rounded_mantissa = s6_rounded_mantissa_temp[22:0];
        s6_rounded_exponent = s6_normalized_exponent;
    end

    s6_exponent_overflow = (s6_rounded_exponent > 9'd254) & !s6_op_is_subtraction;
    s6_exponent_underflow = ((s6_rounded_exponent == 0) | s6_rounded_exponent[8]) & s6_op_is_subtraction;
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
        //guard <= 0;
        //round <= 0;
        //sticky <= 0;
        //normalized_mantissa <= s6_normalized_mantissa;
    end else begin
        //guard <= s6_normalized_guard;
        //round <= s6_normalized_round;
        //sticky <= s6_normalized_sticky;
        //normalized_mantissa<= s6_normalized_mantissa;
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
                    out = {s6_larger_number_sign, 8'hFE, 23'h7FFFFF};
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