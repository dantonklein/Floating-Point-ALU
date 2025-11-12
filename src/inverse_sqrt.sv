import fp_pkg::*;

module mantissa_inverse_sqrt_LUT (
    input logic clk, rst,
    input logic[7:0] in,
    input logic exponent_is_even,
    output logic[23:0] out
);
    logic[23:0] lut_normal[256];
    logic[23:0] lut_double[256];
    initial begin
        $readmemh("inverse_sqrt_lut256_normal.mem", lut_normal);
        $readmemh("inverse_sqrt_lut256_double.mem", lut_double);
    end
    logic[23:0] lut_normal_out, lut_double_out;
    always_ff @(posedge clk) begin
        if(rst) begin
            lut_normal_out <= 0;
            lut_double_out <= 0;
        end else begin
            lut_normal_out <= lut_normal[in];
            lut_double_out <= lut_double[in];
        end
    end
    assign out = exponent_is_even ? lut_double_out : lut_normal_out;
endmodule

module mantissa_inverse_sqrt (
    input logic clk, rst, valid_data_in,
    //input is Q2.23
    input logic[22:0] in,
    input logic[2:0] rounding_mode,
    input logic sign,
    input logic[31:0] special_result,
    input logic input_is_invalid,
    input logic input_is_flushed,
    input logic special_case,
    input logic exponent_is_even,
    output logic[55:0] out,
    output logic valid_data_out,
    output logic[2:0] rounding_mode_out,
    output logic sign_out,
    output logic[31:0] special_result_out,
    output logic input_is_invalid_out,
    output logic input_is_flushed_out,
    output logic special_case_out
);
//input is in [1,4) since it could be scaled
//output is in (0.5,1]
//the pipeline: 1 cycle of lookup table and 2 iterations of newtons method xn+1 = 1/2 * xn(3-(a * xn^2)) which take  cycles each
//stage 0: look up table
//FIRST NEWTONS METHOD ITERATION
//stage 1-2: w = x_n ^ 2
//stage 3-4: y = a * w
//stage 5: z = 3 - y
//stage 6-7: x_n+1 = 1/2*x_n*z
//SECOND NEWTONS METHOD ITERATION
//stage 8-9: w2 = x_n+1 ^2
//stage 10-11: y2 = a * w2
//stage 12: z2 = 3 - y2
//stage 13-14: out = 1/2*x_n+1*z2
logic valid_data_ins[16];
logic[2:0] rounding_modes[16];
logic signs[16];
fp_32b_t special_results[16];
logic input_is_invalids[16];
logic input_is_flusheds[16];
logic special_cases[16];

assign valid_data_ins[0] = valid_data_in;
assign rounding_modes[0] = rounding_mode;
assign signs[0] = sign;
assign special_results[0] = special_result;
assign input_is_invalids[0] = input_is_invalid;
assign input_is_flusheds[0] = input_is_flushed;
assign special_cases[0] = special_case;

genvar i;
generate
    for(i = 0; i < 15; i++) begin
        always_ff @(posedge clk or posedge rst) begin
            if(rst) begin
                valid_data_ins[i+1] <= 0;
                rounding_modes[i+1] <= 0;
                signs[i+1] <= 0;
                special_results[i+1] <= 0;
                input_is_invalids[i+1] <= 0;
                input_is_flusheds[i+1] <= 0;
                special_cases[i+1] <= 0;
            end else begin
                valid_data_ins[i+1] <= valid_data_ins[i];
                rounding_modes[i+1] <= rounding_modes[i];
                signs[i+1] <= signs[i];
                special_results[i+1] <= special_results[i];
                input_is_invalids[i+1] <= input_is_invalids[i];
                input_is_flusheds[i+1] <= input_is_flusheds[i];
                special_cases[i+1] <= special_cases[i];
            end
        end
end
endgenerate
//two situations
//input is in [2, 4) exponent is even
//input is in [1, 2) exponent is odd
//stage 0: read from lookup table
logic[7:0] s0_input_slice;
assign s0_input_slice = in[22:15];

//fixed point Q2.26
logic[27:0] s1_x_n, s2_x_n, s3_x_n, s4_x_n, s5_x_n, s6_x_n;
//fixed point Q2.23
logic[23:0] s1_x_n_pre_extend;
mantissa_inverse_sqrt_LUT s0_inverse_sqrt_lut(.clk(clk), .rst(rst), .exponent_is_even(exponent_is_even), .in(s0_input_slice), .out(s1_x_n_pre_extend));
assign s1_x_n = {1'b0, s1_x_n_pre_extend, 3'b000};

//fixed point Q2.26
logic[27:0] s1_a, s2_a, s3_a, s4_a, s5_a, s6_a, s7_a, s8_a, s9_a, s10_a;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s1_a <= 0;
        s2_a <= 0;
        s3_a <= 0;
        s4_a <= 0;
        s5_a <= 0;
        s6_a <= 0;
        s7_a <= 0;
        s8_a <= 0;
        s9_a <= 0;
        s10_a <= 0;
    end else begin
        s1_a <= exponent_is_even ? {2'b1, in, 4'b0000} : {2'b01, in, 3'b000};
        s2_a <= s1_a;
        s3_a <= s2_a;
        s4_a <= s3_a;
        s5_a <= s4_a;
        s6_a <= s5_a;
        s7_a <= s6_a;
        s8_a <= s7_a;
        s9_a <= s8_a;
        s10_a <= s9_a;
    end
end

//stage 1: w = x_n ^ 2

//fixed point q4.52
logic[55:0] s3_w;
multiplier_delayed #(.WIDTH(28)) s1_mult(.clk(clk), .rst(rst), .in1(s1_x_n), .in2(s1_x_n), .out(s3_w));

//stage 3: y = a * w
//truncate back to Q2.26
logic[27:0] s3_w_truncated;
assign s3_w_truncated = s3_w[53:26];

//fixed point q4.52
logic[55:0] s5_y;
multiplier_delayed #(.WIDTH(28)) s3_mult(.clk(clk), .rst(rst), .in1(s3_a), .in2(s3_w_truncated), .out(s5_y));

//stage 5 z = 3 - y

//fixed point Q2.26
logic [27:0] s5_y_truncated_and_negated;
logic [27:0] three;

assign s5_y_truncated_and_negated = ~(s5_y[53:26]) + 1'b1;
assign three = 28'hC000000;

//fixed point Q2.26
logic [27:0] s5_z, s6_z;
KSA_nbits #(.WIDTH(28)) s3_subtractor(.in1(three), .in2(s5_y_truncated_and_negated), .out(s5_z));

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s2_x_n <= 0;
        s3_x_n <= 0;
        s4_x_n <= 0;
        s5_x_n <= 0;
        s6_x_n <= 0;
        s6_z <= 0;
    end else begin
        s2_x_n <= s1_x_n;
        s3_x_n <= s2_x_n;
        s4_x_n <= s3_x_n;
        s5_x_n <= s4_x_n;
        s6_x_n <= s5_x_n;
        s6_z <= s5_z;
    end
end

//stage 6: x_n+1 = 1/2*x_n*z
logic [27:0] s6_x_n_shifted;
assign s6_x_n_shifted = s6_x_n >> 1;

//fixed point Q4.52
logic[55:0] s8_x_n_1_pre_truncate;
multiplier_delayed #(.WIDTH(28)) s6_mult(.clk(clk), .rst(rst), .in1(s6_x_n_shifted), .in2(s6_z), .out(s8_x_n_1_pre_truncate));

//fixed point Q2.26
logic[27:0] s8_x_n_1;
assign s8_x_n_1 = s8_x_n_1_pre_truncate[53:26];

//stage 8: w2 = x_n+1^2
logic[55:0] s10_w2;
multiplier_delayed #(.WIDTH(28)) s8_mult(.clk(clk), .rst(rst), .in1(s8_x_n_1), .in2(s8_x_n_1), .out(s10_w2));

//stage 10: y2 = a * w2
//truncate back to Q2.26
logic[27:0] s10_w2_truncated;
assign s10_w2_truncated = s10_w2[53:26];

//fixed point q4.52
logic[55:0] s12_y2;
multiplier_delayed #(.WIDTH(28)) s10_mult(.clk(clk), .rst(rst), .in1(s10_a), .in2(s10_w2_truncated), .out(s12_y2));

//stage 12: z2 = 3 - y2

//fixed point Q2.26
logic[27:0] s12_y2_truncated_and_negated;

assign s12_y2_truncated_and_negated = ~(s12_y2[53:26]) + 1'b1;

logic[27:0] s12_z2;
KSA_nbits #(.WIDTH(28)) s8_subtractor(.in1(three), .in2(s12_y2_truncated_and_negated), .out(s12_z2));

logic[27:0] s13_z2;
logic[27:0] s9_x_n_1, s10_x_n_1, s11_x_n_1, s12_x_n_1, s13_x_n_1;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s13_z2 <= 0;
        s9_x_n_1 <= 0;
        s10_x_n_1 <= 0;
        s11_x_n_1 <= 0;
        s12_x_n_1 <= 0;
        s13_x_n_1 <= 0;
    end else begin
        s13_z2 <= s12_z2;
        s9_x_n_1 <= s8_x_n_1;
        s10_x_n_1 <= s9_x_n_1;
        s11_x_n_1 <= s10_x_n_1;
        s12_x_n_1 <= s11_x_n_1;
        s13_x_n_1 <= s12_x_n_1;
    end
end

//stage 13: out = 1/2*x_n+1*z2
logic [27:0] s13_x_n_1_shifted;
assign s13_x_n_1_shifted = s13_x_n_1 >> 1;
multiplier_delayed #(.WIDTH(28)) s13_mult(.clk(clk), .rst(rst), .in1(s13_x_n_1_shifted), .in2(s13_z2), .out(out));

assign valid_data_out = valid_data_ins[15];
assign rounding_mode_out = rounding_modes[15];
assign sign_out = signs[15];
assign special_result_out = special_results[15];
assign input_is_invalid_out = input_is_invalids[15]; 
assign input_is_flushed_out = input_is_flusheds[15];
assign special_case_out = special_cases[15];

endmodule

module fp_inverse_sqrt_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation, division_by_zero,
    output logic valid_data_out
);
//Stage 1: Denorm, NaN, Zero, Infinity processing
//input flag handling
fp_32b_t s1_in_init;
assign s1_in_init = in;

//special case handling
logic s1_in_iszero;
logic s1_in_isinfinite;
logic s1_in_isqnan;
logic s1_in_issnan;
logic s1_in_isdenorm;
//special cases for input

always_comb begin
    s1_in_iszero = (s1_in_init.exponent == '0) & (s1_in_init.mantissa == '0);
    s1_in_isinfinite = (s1_in_init.exponent == '1) & (s1_in_init.mantissa == '0);
    s1_in_isqnan = (s1_in_init.exponent == '1) & (s1_in_init.mantissa != '0) & s1_in_init.mantissa[22];
    s1_in_issnan = (s1_in_init.exponent == '1) & (s1_in_init.mantissa != '0) & ~s1_in_init.mantissa[22];
    s1_in_isdenorm = (s1_in_init.exponent == '0) & (s1_in_init.mantissa != '0);
end
logic s1_input_is_invalid;
logic s1_input_is_flushed;
assign s1_input_is_invalid = s1_in_issnan;
assign s1_input_is_flushed = s1_in_isdenorm;

fp_32b_t s2_special_result, s2_in;
logic s2_input_is_invalid;
logic s2_input_is_flushed;
logic s2_special_case;
logic s2_valid_data_in;
logic s2_division_by_zero;
logic[2:0] s2_rounding_mode;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s2_in <= '0;
        s2_input_is_invalid <= 0;
        s2_input_is_flushed <= 0;
        s2_valid_data_in <= 0;
        s2_rounding_mode <= '0;
        s2_special_case <= 0;
        s2_special_result <= '0;
        s2_division_by_zero <= 0;
    end else begin
        s2_in <= s1_in_init;
        s2_input_is_invalid <= s1_input_is_invalid;
        s2_input_is_flushed <= s1_input_is_flushed;
        s2_valid_data_in <= valid_data_in;
        s2_rounding_mode <= rounding_mode;
        s2_division_by_zero <= (s1_in_iszero | s1_in_isdenorm) & ~s1_input_is_invalid;
        //Special cases:

        //propagate qnan
        if(s1_in_isqnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in_init;
        //convert snan to qnan
        end else if(s1_in_issnan) begin
            s2_special_case <= 1;
            s2_special_result <= s1_in_init | 32'b00000000010000000000000000000000;
        //when number is negative
        end else if(s1_in_init.sign) begin
            s2_special_case <= 1;
            s2_special_result <= 32'hFFC00000;
        //when infinity
        end else if(s1_in_isinfinite) begin
            s2_special_case <= 1;
            s2_special_result <= 0;

        //when zero
        end else if(s1_in_iszero | s1_in_isdenorm) begin
            s2_special_case <= 1;
            s2_special_result <= 32'h7F800000;

        end else begin
            s2_special_case <= 0;
            //this doesnt matter, but its there to show intent that the special result doesnt get propagated since special case is zero
            s2_special_result <= '0;
        end
    end
end
//stage 2 begin mantissa and exponent calculation
logic s2_exponent_is_even;

//if the biased exponent is even, the actual exponent is odd and therefore cant be divided by 2 evenly
//this requires the mantissa to be 2xed, making the exponent odd
assign s2_exponent_is_even = ~s2_in.exponent[0];

logic[7:0] s3_exponent; 
logic s3_exponent_is_even;
logic s3_division_by_zero;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_exponent_is_even <= 0;
        s3_exponent <= 0;
        s3_division_by_zero <= 0;
    end else begin
        s3_exponent_is_even <= s2_exponent_is_even;
        s3_exponent <= s2_in.exponent;
        s3_division_by_zero <= s2_division_by_zero;
    end
end

//stage 3 new exponent calculation
logic[8:0] s3_new_exponent;
always_comb begin
    if(s3_exponent_is_even) begin
        s3_new_exponent = ((9'd380 - s3_exponent) >> 1);
    end else begin
        s3_new_exponent = ((9'd379 - s3_exponent) >> 1);
    end
end
logic s4_s17_division_by_zero[14];
logic[7:0] s4_s17_new_exponents[14];

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        for(int i = 0; i < 14; i++) begin
            s4_s17_new_exponents[i] <= 0;
            s4_s17_division_by_zero[i] <= 0;
        end
    end else begin
        if(s3_new_exponent[8]) begin
            s4_s17_new_exponents[0] <= 0;
        end else begin
            s4_s17_new_exponents[0] <= s3_new_exponent[7:0];
        end
        s4_s17_division_by_zero[0] <= s3_division_by_zero;
        for(int i = 0; i < 13; i++) begin
            s4_s17_new_exponents[i+1] <= s4_s17_new_exponents[i];
            s4_s17_division_by_zero[i+1] <= s4_s17_division_by_zero[i];
        end
    end
end

//stage 17, final rounding
//Q4.52
logic[55:0] s17_reciprocal_out;
logic[22:0] s17_mantissa_pre_round;
logic s17_valid_data_out;
logic[2:0] s17_rounding_mode;
fp_32b_t s17_special_result;
logic s17_input_is_invalid;
logic s17_input_is_flushed;
logic s17_special_case;
logic s17_sign, s17_guard, s17_round, s17_sticky;

mantissa_inverse_sqrt s17_inverse_square_root(.clk(clk), .rst(rst), .valid_data_in(s2_valid_data_in), .in(s2_in.mantissa), .rounding_mode(s2_rounding_mode), .sign(s2_in.sign),
.special_result(s2_special_result), .input_is_invalid(s2_input_is_invalid), .input_is_flushed(s2_input_is_flushed), .special_case(s2_special_case), .exponent_is_even(s2_exponent_is_even), .out(s17_reciprocal_out),
.valid_data_out(s17_valid_data_out), .rounding_mode_out(s17_rounding_mode), .special_result_out(s17_special_result), .input_is_invalid_out(s17_input_is_invalid), 
.input_is_flushed_out(s17_input_is_flushed), .special_case_out(s17_special_case), .sign_out(s17_sign));

logic s17_output_is_1;
//output will be in (0.5,1], we need to renormalize it into having the leading bit 1. the output is 23 bits with an implicit 1 at the start

always_comb begin
    s17_output_is_1 = s17_reciprocal_out[52];
    if(s17_output_is_1) begin 
        s17_mantissa_pre_round = s17_reciprocal_out[51:29];
        s17_guard = s17_reciprocal_out[28];
        s17_round = s17_reciprocal_out[27];
        s17_sticky = | s17_reciprocal_out[26:0];
    end else begin
        s17_mantissa_pre_round = s17_reciprocal_out[50:28];
        s17_guard = s17_reciprocal_out[27];
        s17_round = s17_reciprocal_out[26];
        s17_sticky = | s17_reciprocal_out[25:0];
    end
end

logic s17_exponent_overflow, s17_exponent_underflow, s17_has_grs_bits;
logic[23:0] s17_rounded_mantissa_temp;
logic[22:0] s17_rounded_mantissa;
logic[7:0] s17_exponent;
logic[8:0] s17_rounded_exponent;
assign s17_exponent = s4_s17_new_exponents[13];
floating_point_rounder rounder(.mantissa(s17_mantissa_pre_round), .guard(s17_guard), .round(s17_round), .sticky(s17_sticky),
.sign(s17_sign), .rounding_mode(s17_rounding_mode), .rounded_mantissa_pre_overflow_detection(s17_rounded_mantissa_temp));
always_comb begin
    if(s17_rounded_mantissa_temp[23]) begin
        s17_rounded_mantissa = 23'd0;
        s17_rounded_exponent = {1'b0,s17_exponent} + 9'd1;
    end else begin
        s17_rounded_mantissa = s17_rounded_mantissa_temp[22:0];
        s17_rounded_exponent = {1'b0,s17_exponent};
    end


    s17_exponent_overflow = (s17_rounded_exponent > 9'd254);
    s17_exponent_underflow = (s17_rounded_exponent == 9'd0);
    s17_has_grs_bits = s17_guard | s17_round | s17_sticky;
end

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        out <= '0;
        overflow <= 0;
        underflow <= 0;
        inexact <= 0;
        invalid_operation <= 0;
        valid_data_out <= 0;
        division_by_zero <= 0;
    end else begin
        invalid_operation <= s17_input_is_invalid;
        valid_data_out <= s17_valid_data_out;
        division_by_zero <= s4_s17_division_by_zero[13];
        if(s17_special_case) begin
            overflow <= 1'b0;
            inexact <= 1'b0;
            underflow <= s17_input_is_flushed;
            out <= s17_special_result;
        end else if(s17_exponent_overflow) begin
            overflow <= 1'b1;
            underflow <= 1'b0;
            inexact <= 1'b1;
            case(s17_rounding_mode)
                RTZ: begin
                    out <= {s17_sign, 8'hFE, 23'h7FFFFF};
                end
                RDN: begin
                    if(s17_sign) begin
                        out <= {1'b1, 8'hFF, 23'h0};
                    end else begin
                        out <= {1'b0, 8'hFE, 23'h7FFFFF};
                    end
                end
                RUP: begin
                    if(s17_sign) begin
                        out <= {1'b1, 8'hFE, 23'h7FFFFF};
                    end else begin
                        out <= {1'b0, 8'hFF, 23'h0};
                    end
                end
                default: begin
                    out <= {s17_sign, 8'hFF, 23'h0};
                end
            endcase
        end else if(s17_exponent_underflow) begin
            overflow <= 1'b0;
            underflow <= s17_has_grs_bits;
            inexact <= 1'b1;
            out <= {s17_sign, 8'h0, 23'h0};
        end else begin
            overflow <= 1'b0;
            underflow <= 1'b0;
            inexact <= s17_has_grs_bits;
            out <= {s17_sign, s17_rounded_exponent[7:0],  s17_rounded_mantissa};
        end
    end
end
endmodule