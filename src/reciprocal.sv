import fp_pkg::*;

module mantissa_reciprocal_24bit_LUT (
    input logic clk, rst,
    input logic[7:0] in,
    output logic[23:0] out
);
    logic[23:0] lut[256];

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

module mantissa_reciprocal_24bit (
    input logic clk, rst, valid_data_in,
    input logic[22:0] in,
    input logic[2:0] rounding_mode,
    input logic sign,
    input fp_32b_t special_result,
    input logic input_is_invalid,
    input logic input_is_flushed,
    input logic special_case,
    output logic[22:0] out,
    output logic valid_data_out,
    output logic[2:0] rounding_mode_out,
    output logic sign_out,
    output fp_32b_t special_result_out,
    output logic input_is_invalid_out,
    output logic input_is_flushed_out,
    output logic special_case_out,
    output logic guard, round, sticky 
);
//whats cool about this reciprocal is that the inputs will always be in [1,2)
//the lut output will be in (0.5,1]
//the pipeline: 1 cycle of lookup table and 2 iterations of newtons method xn+1 = xn(2-axn) which take 7 cycles each
//input is Q1.23 wide
//stage 0: look up table
//FIRST NEWTONS METHOD ITERATION
//stage 1-2: y = a * xn
//stage 3: z = 2 - y
//stage 4: round
//stage 5-6: xn+1 = xn * z
//stage 7: round
//SECOND NEWTONS METHOD ITERATION
//stage 8-9 y2 = a * xn+1
//stage 10 z2 = 2 - y2
//stage 11 round
//stage 12-13: out = xn+1 * z2

//handle valid_data_flag and have rounding_modes available on all stages
logic valid_data_ins[15];
logic[2:0] rounding_modes[15];
logic signs[15];
fp_32b_t special_results[15];
logic input_is_invalids[15];
logic input_is_flusheds[15];
logic special_cases[15];

assign valid_data_ins[0] = valid_data_in;
assign rounding_modes[0] = rounding_mode;
assign signs[0] = sign;
assign special_results[0] = special_result;
assign input_is_invalids[0] = input_is_invalid;
assign input_is_flusheds[0] = input_is_flushed;
assign special_cases[0] = special_case;


genvar i;
generate
    for(i = 0; i < 14; i++) begin
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
mantissa_reciprocal_24bit_LUT s0_reciprocal_lut(.clk(clk), .rst(rst), .in(s0_input_slice), .out(s1_x_n));

//fixed point Q1.23
logic[23:0] s1_x_n, s2_x_n, s3_x_n, s4_x_n, s5_x_n;
logic[23:0] s1_a, s2_a, s3_a, s4_a, s5_a, s6_a, s7_a, s8_a;
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
    end else begin
        s1_a <= {1'b1,in};
        s2_a <= s1_a;
        s3_a <= s2_a;
        s4_a <= s3_a;
        s5_a <= s4_a;
        s6_a <= s5_a;
        s7_a <= s6_a;
        s8_a <= s7_a;
    end
end

//stage 1: y = a * xn


//fixed point Q2.46
logic [47:0] s3_y;
Dadda_Multiplier_24bit_pipelined s1_mult(.clk(clk), .rst(rst), in1(s1_a), in2(s1_x_n), out(s3_y));


//stage 3: z = 2 - y

//fixed point Q2.26
logic [27:0] s3_y_truncated_and_negated;
logic [27:0] two;

//the reason 3 extra bits to the right of the decimal point are maintained is to avoid needing to round
assign s3_y_truncated_and_negated = ~(s3_y[47:20]) + 1'b1;
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
        s5_x_n <= 0;
        s4_z <= 0;
    end else begin
        s2_x_n <= s1_x_n;
        s3_x_n <= s2_x_n;
        s4_x_n <= s3_x_n;
        s5_x_n <= s4_x_n;
        s4_z <= s3_z[26:0];
    end
end
//stage 4
logic s4_guard, s4_round, s4_sticky;
//fixed point Q1.23
logic[23:0] s4_z_rounder_input, s4_z_rounded;
logic[2:0] s4_rounding_mode;
logic s4_sign;

assign {s4_z_rounder_input, s4_guard, s4_round, s4_sticky} = s4_z;
assign s4_rounding_mode = rounding_modes[4];
assign s4_sign = signs[4];

q1_23_fixed_point_rounder s4_rounder(.in(s4_z_rounder_input), .guard(s4_guard), .round(s4_round), .sticky(s4_sticky), .sign(s4_sign), .rounding_mode(s4_rounding_mode), .out(s4_z_rounded));

logic[23:0] s5_z;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s5_x_n <= 0;
        s5_z <= 0;
    end else begin
        s5_x_n <= s4_x_n;
        s5_z <= s4_z_rounded;
    end
end

//stage 5

//fixed point Q2.46
logic[47:0] s7_x_n_1;
Dadda_Multiplier_24bit_pipelined s5_mult(.clk(clk), .rst(rst), in1(s5_x_n), in2(s5_z), out(s7_x_n_1));

//stage 7

logic s7_guard, s7_round, s7_sticky;
//fixed point Q1.23
logic[23:0] s7_x_n_1_rounder_input, s7_x_n_1_rounded;
logic[2:0] s7_rounding_mode;
logic s7_sign;
assign s7_rounding_mode = rounding_modes[7];
assign s7_sign = signs[7];


assign s7_x_n_1_rounder_input = s7_x_n_1[46:23];
assign s7_guard = s7_x_n_1[22];
assign s7_round = s7_x_n_1[21];
assign s7_sticky = | s7_x_n_1[20:0];


q1_23_fixed_point_rounder s7_rounder(.in(s7_x_n_1_rounder_input), .guard(s7_guard), .round(s7_round), .sticky(s7_sticky), .sign(s7_sign), .rounding_mode(s7_rounding_mode), .out(s7_x_n_1_rounded));

logic[23:0] s8_x_n_1;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s8_x_n_1 <= 0;
    end else begin
        s8_x_n_1 <= s7_x_n_1_rounded;
    end
end
//stage 8 y2 = a * xn+1



//fixed point Q2.46
logic [47:0] s10_y2;
Dadda_Multiplier_24bit_pipelined s8_mult(.clk(clk), .rst(rst), in1(s8_a), in2(s8_x_n_1), out(s10_y2));

//stage 10: z2 = 2 - y2

logic [27:0] s10_y_truncated_and_negated

assign s10_y_truncated_and_negated = ~(s10_y2[47:20]) + 1'b1;

//fixed point Q2.26
logic [27:0] s10_z;
KSA_nbits #(.WIDTH(28)) s10_subtractor(.in1(two), .in2(s10_y_truncated_and_negated), .out(s10_z));

//fixed point Q1.26
logic [26:0] s11_z;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s11_z <= 0;
    end else begin
        s11_z <= s10_z[26:0];
    end
end

//stage 11
logic s11_guard, s11_round, s11_sticky;
//fixed point Q1.23
logic[23:0] s11_z_rounder_input, s11_z_rounded;
logic[2:0] s11_rounding_mode;
logic s11_sign;

assign {s11_z_rounder_input, s11_guard, s11_round, s11_sticky} = s11_z;
assign s11_rounding_mode = rounding_modes[11];
assign s11_sign = signs[11];

q1_23_fixed_point_rounder s11_rounder(.in(s11_z_rounder_input), .guard(s11_guard), .round(s11_round), .sticky(s11_sticky), .sign(s11_sign), .rounding_mode(s11_rounding_mode), .out(s11_z_rounded));

logic[23:0] s12_z;
logic[23:0] s9_x_n_1, s10_x_n_1, s11_x_n_1, s12_x_n_1; 
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s12_z <= 0;
        s9_x_n_1 <= 0;
        s10_x_n_1 <= 0;
        s11_x_n_1 <= 0;
        s12_x_n_1 <= 0;
    end else begin
        s12_z <= s11_z_rounded;
        s9_x_n_1 <= s8_x_n_1;
        s10_x_n_1 <= s9_x_n_1;
        s11_x_n_1 <= s10_x_n_1;
        s12_x_n_1 <= s11_x_n_1;
    end
end

//fixed point Q2.46
logic[47:0] s14_out;
logic s14_output_is_1;
Dadda_Multiplier_24bit_pipelined s12_mult(.clk(clk), .rst(rst), in1(s12_z), in2(s12_x_n_1), out(s14_out));

//output will be in (0.5,1], we need to renormalize it into having the leading bit 1. the output is 23 bits with an implicit 1 at the start
always_comb begin
    s14_output_is_1 = s14_out[46];
    if(output_is_1) begin 
        out = s14_out[45:23];
        guard = s14_out[22];
        round = s14_out[21];
        sticky = | s14_out[20:0];
    end else begin
        out = s14_out[44:22];
        guard = s14_out[21];
        round = s14_out[20];
        sticky = | s14_out[19:0];
    end
end


assign valid_data_out = valid_data_ins[14];
assign rounding_mode_out = rounding_modes[14];
assign sign_out = signs[14];
assign special_result_out = special_results[14];
assign input_is_invalid_out = input_is_invalids[14]; 
assign input_is_flushed_out = input_is_flusheds[14];
assign special_case_out = special_cases[14];

endmodule

module fp_reciprocal_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation,
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

fp_32b_t s2_special_result, s2_in,
logic s2_input_is_invalid;
logic s2_input_is_flushed;
logic s2_special_case;
logic s2_valid_data_in;
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
    end else begin
        s2_in <= s1_in1_init;
        s2_input_is_invalid <= s1_input_is_invalid;
        s2_input_is_flushed <= s1_input_is_flushed;
        s2_valid_data_in <= valid_data_in;
        s2_rounding_mode <= rounding_mode;

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
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_mantissa_is_one <= 0;
        s3_exponent <= 0;
    end else begin
        //if the mantissa is 1
        s3_mantissa_is_one <= (s2_in.mantissa == 23'd0);
        s3_exponent <= s2_in.exponent;
    end
end

logic[7:0] s4_s16_new_exponents[13];
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s4_s16_new_exponents[0] <= 0;
    end else begin
        if(s3_mantissa_is_one) begin
        s4_s16_new_exponents[0] <= 8'254 - s3_exponent;
        end else begin
        s4_s16_new_exponents[0] <= 8'253 - s3_exponent;
        end
    end
end

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        for(int i = 0; i < 13; i++) begin
            s4_s16_new_exponents[i+1] <= 0;
        end
    end else begin
        for(int i = 0; i < 13; i++) begin
            s4_s16_new_exponents[i+1] <= s4_s16_new_exponents[i];
        end
    end
end

logic[22:0] s16_mantissa_pre_round;
logic s16_valid_data_out;
logic s16_rounding_mode;
fp_32b_t s16_special_result;
logic s16_input_is_invalid;
logic s16_input_is_flushed;
logic s16_special_case;
logic s16_sign, s16_guard, s16_round, s16_sticky;
//i stuffed all the signals that need to be propagated in the module since i already needed later versions of the rounding mode and sign
mantissa_reciprocal_24bit s2_reciprocal(.clk(clk), .rst(rst), .valid_data_in(s2_valid_data_in), .in(s2_in.mantissa), .rounding_mode(s2_rounding_mode), .sign(s2_in.sign),
.special_result(s2_special_result), .input_is_invalid(s2_input_is_invalid), .input_is_flushed(s2_input_is_flushed), .special_case(s2_special_case), .out(s16_mantissa_pre_round),
.valid_data_out(s16_valid_data_out), .rounding_mode_out(s16_rounding_mode), .special_result_out(s16_special_result), .input_is_invalid_out(s16_input_is_invalid), 
.input_is_flushed_out(s16_input_is_flushed), .special_case_out(s16_special_case), .sign_out(s16_sign), .guard(s16_guard), .round(s16_round), .sticky(s16_sticky));

endmodule