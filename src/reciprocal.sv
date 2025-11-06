import fp_pkg::*;

module mantissa_reciprocal_27bit_LUT (
    input logic clk, rst,
    input logic[7:0] in,
    output logic[26:0] out
);
    logic[26:0] lut[256];

    initial begin
        $readmemh("reciprocal_lut256.mem", lut);
    end

    always_ff @(posedge clk) begin
        if(rst) begin
            out <= 0;
        end else begin
            out <= lut[in];
        end
    end
endmodule


module mantissa_reciprocal (
    input logic clk, rst, valid_data_in,
    input logic[22:0] in,
    input logic[2:0] rounding_mode,
    input logic sign,
    input logic[31:0] special_result,
    input logic input_is_invalid,
    input logic input_is_flushed,
    input logic special_case,
    output logic[53:0] out,
    output logic valid_data_out,
    output logic[2:0] rounding_mode_out,
    output logic sign_out,
    output logic[31:0] special_result_out,
    output logic input_is_invalid_out,
    output logic input_is_flushed_out,
    output logic special_case_out
);
//whats cool about this reciprocal is that the inputs will always be in [1,2)
//the lut output will be in (0.5,1]
//the pipeline: 1 cycle of lookup table and 2 iterations of newtons method xn+1 = xn(2-axn) which take 5 cycles each
//input is Q1.23 wide
//stage 0: look up table
//FIRST NEWTONS METHOD ITERATION
//stage 1-2: y = a * xn
//stage 3: z = 2 - y
//stage 4-5: xn+1 = xn * z
//SECOND NEWTONS METHOD ITERATION
//stage 6-7 y2 = a * xn+1
//stage 8 z2 = 2 - y2
//stage 9-10: out = xn+1 * z2
logic valid_data_ins[12];
logic[2:0] rounding_modes[12];
logic signs[12];
fp_32b_t special_results[12];
logic input_is_invalids[12];
logic input_is_flusheds[12];
logic special_cases[12];

assign valid_data_ins[0] = valid_data_in;
assign rounding_modes[0] = rounding_mode;
assign signs[0] = sign;
assign special_results[0] = special_result;
assign input_is_invalids[0] = input_is_invalid;
assign input_is_flusheds[0] = input_is_flushed;
assign special_cases[0] = special_case;

genvar i;
generate
    for(i = 0; i < 11; i++) begin
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

//stage 0: read from lookup table
logic[7:0] s0_input_slice;
assign s0_input_slice = in[22:15];


//fixed point Q1.26
logic[26:0] s1_x_n, s2_x_n, s3_x_n, s4_x_n;

mantissa_reciprocal_27bit_LUT s0_reciprocal_lut(.clk(clk), .rst(rst), .in(s0_input_slice), .out(s1_x_n));

logic[26:0] s1_a, s2_a, s3_a, s4_a, s5_a, s6_a;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s1_a <= 0;
        s2_a <= 0;
        s3_a <= 0;
        s4_a <= 0;
        s5_a <= 0;
        s6_a <= 0;
    end else begin
        s1_a <= {1'b1, in, 3'b000};
        s2_a <= s1_a;
        s3_a <= s2_a;
        s4_a <= s3_a;
        s5_a <= s4_a;
        s6_a <= s5_a;
    end
end

//stage 1: y = a * xn

//fixed point Q2.52
logic [53:0] s3_y;
multiplier_delayed #(.WIDTH(27)) s1_mult(.clk(clk), .rst(rst), .in1(s1_a), .in2(s1_x_n), .out(s3_y));

//stage 3: z = 2 - y

//fixed point Q2.26
logic [27:0] s3_y_truncated_and_negated;
logic [27:0] two;

//the reason 3 extra bits to the right of the decimal point are maintained is to avoid needing to round
assign s3_y_truncated_and_negated = ~(s3_y[53:26]) + 1'b1;
assign two = 28'h8000000;

//fixed point Q2.26
logic [27:0] s3_z;
KSA_nbits #(.WIDTH(28)) s3_subtractor(.in1(two), .in2(s3_y_truncated_and_negated), .out(s3_z));

//fixed point Q1.26
logic [26:0] s4_z;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s2_x_n <= 0;
        s3_x_n <= 0;
        s4_x_n <= 0;
        s4_z <= 0;
    end else begin
        s2_x_n <= s1_x_n;
        s3_x_n <= s2_x_n;
        s4_x_n <= s3_x_n;
        s4_z <= s3_z[26:0];
    end
end

//stage 4 xn+1 = xn * z
//fixed point Q2.52
logic[53:0] s6_x_n_1_pre_truncate;

multiplier_delayed #(.WIDTH(27)) s4_mult (.clk(clk), .rst(rst), .in1(s4_x_n), .in2(s4_z), .out(s6_x_n_1_pre_truncate));

//stage 6 y2 = a * xn+1
logic[26:0] s6_x_n_1;
assign s6_x_n_1 = s6_x_n_1_pre_truncate[52:26]; 


logic[53:0] s8_y2_pre_truncate;
multiplier_delayed #(.WIDTH(27)) s6_mult (.clk(clk), .rst(rst), .in1(s6_x_n_1), .in2(s6_a), .out(s8_y2_pre_truncate));

//stage 8 z2 = 2 - y2
logic[27:0] s8_y2_truncated_and_negated;

assign s8_y2_truncated_and_negated = ~(s8_y2_pre_truncate[53:26]) + 1'b1;

//fixed point Q2.26
logic[27:0] s8_z2;
KSA_nbits #(.WIDTH(28)) s8_subtractor(.in1(two), .in2(s8_y2_truncated_and_negated), .out(s8_z2));

//fixed point Q1.26
logic[26:0] s9_z2;
logic[26:0] s7_x_n_1, s8_x_n_1, s9_x_n_1;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s9_z2 <= 0;
        s7_x_n_1 <= 0;
        s8_x_n_1 <= 0;
        s9_x_n_1 <= 0;
    end else begin
        s9_z2 <= s8_z2[26:0];
        s7_x_n_1 <= s6_x_n_1;
        s8_x_n_1 <= s7_x_n_1;
        s9_x_n_1 <= s8_x_n_1;
    end
end

//stage 9 out = xn+1 * z2
//fixed point Q2.52

multiplier_delayed #(.WIDTH(27)) s9_mult (.clk(clk), .rst(rst), .in1(s9_x_n_1), .in2(s9_z2), .out(out));

//output will be in (0.5,1], we need to renormalize it into having the leading bit 1. the output is 23 bits with an implicit 1 at the start


assign valid_data_out = valid_data_ins[11];
assign rounding_mode_out = rounding_modes[11];
assign sign_out = signs[11];
assign special_result_out = special_results[11];
assign input_is_invalid_out = input_is_invalids[11]; 
assign input_is_flushed_out = input_is_flusheds[11];
assign special_case_out = special_cases[11];

endmodule

module fp_reciprocal_pipeline (
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

        //when infinity
        end else if(s1_in_isinfinite) begin
            //propagate qnan, exception flag is raised 
            s2_special_case <= 1;
            s2_special_result <= {s1_in_init.sign,31'd0};

        //when zero
        end else if(s1_in_iszero | s1_in_isdenorm) begin
            s2_special_case <= 1;
            s2_special_result <= {s1_in_init.sign,31'h7F800000};

        end else begin
            s2_special_case <= 0;
            //this doesnt matter, but its there to show intent that the special result doesnt get propagated since special case is zero
            s2_special_result <= '0;
        end
    end
end
//stage 2 begin mantissa and exponent calculation

logic[7:0] s3_exponent; 
logic s3_mantissa_is_one;
logic s3_division_by_zero
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_mantissa_is_one <= 0;
        s3_exponent <= 0;
        s3_division_by_zero <= 0;
    end else begin
        //if the mantissa is 1
        s3_mantissa_is_one <= (s2_in.mantissa == 23'd0);
        s3_exponent <= s2_in.exponent;
        s3_division_by_zero <= s2_division_by_zero;
    end
end

//stage 3 exponent calculation
logic[8:0] s3_new_exponent;
always_comb begin
    if(s3_mantissa_is_one) begin
        s3_new_exponent = 9'd254 - {1'b0, s3_exponent};
    end else begin
        s3_new_exponent = 9'd253 - {1'b0, s3_exponent};
    end
end
logic s4_s13_division_by_zero[10];
logic[7:0] s4_s13_new_exponents[10];

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        for(int i = 0; i < 10; i++) begin
            s4_s13_new_exponents[i] <= 0;
            s4_s13_division_by_zero[i] <= 0;
        end
    end else begin
        if(s3_new_exponent[8]) begin
            s4_s13_new_exponents[0] <= 0;
        end else begin
            s4_s13_new_exponents[0] <= s3_new_exponent[7:0];
        end
        s4_s13_division_by_zero[0] <= s3_division_by_zero;
        for(int i = 0; i < 9; i++) begin
            s4_s13_new_exponents[i+1] <= s4_s13_new_exponents[i];
            s4_s13_division_by_zero[i+1] <= s4_s13_division_by_zero[i];
        end
    end
end

//stage 13, final rounding
logic[53:0] s13_reciprocal_out;
logic[22:0] s13_mantissa_pre_round;
logic s13_valid_data_out;
logic[2:0] s13_rounding_mode;
fp_32b_t s13_special_result;
logic s13_input_is_invalid;
logic s13_input_is_flushed;
logic s13_special_case;
logic s13_sign, s13_guard, s13_round, s13_sticky;
//i stuffed all the signals that need to be propagated in the module since i already needed later versions of the rounding mode and sign
mantissa_reciprocal_24bit s13_reciprocal(.clk(clk), .rst(rst), .valid_data_in(s2_valid_data_in), .in(s2_in.mantissa), .rounding_mode(s2_rounding_mode), .sign(s2_in.sign),
.special_result(s2_special_result), .input_is_invalid(s2_input_is_invalid), .input_is_flushed(s2_input_is_flushed), .special_case(s2_special_case), .out(s13_reciprocal_out),
.valid_data_out(s13_valid_data_out), .rounding_mode_out(s13_rounding_mode), .special_result_out(s13_special_result), .input_is_invalid_out(s13_input_is_invalid), 
.input_is_flushed_out(s13_input_is_flushed), .special_case_out(s13_special_case), .sign_out(s13_sign));

logic s13_output_is_1;
//output will be in (0.5,1], we need to renormalize it into having the leading bit 1. the output is 23 bits with an implicit 1 at the start

always_comb begin
    s13_output_is_1 = s13_reciprocal_out[52];
    if(s13_output_is_1) begin 
        s13_mantissa_pre_round = s13_reciprocal_out[51:29];
        s13_guard = s13_reciprocal_out[28];
        s13_round = s13_reciprocal_out[27];
        s13_sticky = | s13_reciprocal_out[26:0];
    end else begin
        s13_mantissa_pre_round = s13_reciprocal_out[50:28];
        s13_guard = s13_reciprocal_out[27];
        s13_round = s13_reciprocal_out[26];
        s13_sticky = | s13_reciprocal_out[25:0];
    end
end

logic s13_exponent_overflow, s13_exponent_underflow, s13_has_grs_bits;
logic[23:0] s13_rounded_mantissa_temp;
logic[22:0] s13_rounded_mantissa;
logic[7:0] s13_exponent;
logic[8:0] s13_rounded_exponent;
assign s13_exponent = s4_s13_new_exponents[9];
floating_point_rounder rounder(.mantissa(s13_mantissa_pre_round), .guard(s13_guard), .round(s13_round), .sticky(s13_sticky),
.sign(s13_sign), .rounding_mode(s13_rounding_mode), .rounded_mantissa_pre_overflow_detection(s13_rounded_mantissa_temp));
always_comb begin
    if(s13_rounded_mantissa_temp[23]) begin
        s13_rounded_mantissa = 23'd0;
        s13_rounded_exponent = {1'b0,s13_exponent} + 9'd1;
    end else begin
        s13_rounded_mantissa = s13_rounded_mantissa_temp[22:0];
        s13_rounded_exponent = {1'b0,s13_exponent};
    end


    s13_exponent_overflow = (s13_rounded_exponent > 9'd254);
    s13_exponent_underflow = (s13_rounded_exponent == 9'd0);
    s13_has_grs_bits = s13_guard | s13_round | s13_sticky;
end

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        out <= '0;
        overflow <= 0;
        underflow <= 0;
        inexact <= 0;
        invalid_operation <= 0;
        valid_data_out <= 0;
    end else begin
        invalid_operation <= s13_input_is_invalid;
        valid_data_out <= s13_valid_data_out;
        division_by_zero <= s4_s13_division_by_zero[9];
        if(s13_special_case) begin
            overflow <= 1'b0;
            inexact <= 1'b0;
            underflow <= s13_input_is_flushed;
            out <= s13_special_result;
        end else if(s13_exponent_overflow) begin
            overflow <= 1'b1;
            underflow <= 1'b0;
            inexact <= 1'b1;
            case(s13_rounding_mode)
                RTZ: begin
                    out <= {s13_sign, 8'hFE, 23'h7FFFFF};
                end
                RDN: begin
                    if(s13_sign) begin
                        out <= {1'b1, 8'hFF, 23'h0};
                    end else begin
                        out <= {1'b0, 8'hFE, 23'h7FFFFF};
                    end
                end
                RUP: begin
                    if(s13_sign) begin
                        out <= {1'b1, 8'hFE, 23'h7FFFFF};
                    end else begin
                        out <= {1'b0, 8'hFF, 23'h0};
                    end
                end
                default: begin
                    out <= {s13_sign, 8'hFF, 23'h0};
                end
            endcase
        end else if(s13_exponent_underflow) begin
            overflow <= 1'b0;
            underflow <= s13_has_grs_bits;
            inexact <= 1'b1;
            out <= {s13_sign, 8'h0, 23'h0};
        end else begin
            overflow <= 1'b0;
            underflow <= 1'b0;
            inexact <= s13_has_grs_bits;
            out <= {s13_sign, s13_rounded_exponent[7:0],  s13_rounded_mantissa};
        end
    end
end
endmodule