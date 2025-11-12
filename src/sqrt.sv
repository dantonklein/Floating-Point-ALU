import fp_pkg::*;

module fp_sqrt_pipeline (
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

fp_32b_t s2_special_result, s2_in;
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
        s2_in <= s1_in_init;
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
        //when number is negative
        end else if(s1_in_init.sign) begin
            s2_special_case <= 1;
            s2_special_result <= 32'hFFC00000;
        //when infinity
        end else if(s1_in_isinfinite) begin
            s2_special_case <= 1;
            s2_special_result <= 32'h7F800000;

        //when zero
        end else if(s1_in_iszero | s1_in_isdenorm) begin
            s2_special_case <= 1;
            s2_special_result <= 0;

        end else begin
            s2_special_case <= 0;
            //im using the special result as the way to carry the initial value to the last multiplication
            s2_special_result <= s1_in_init;
        end
    end
end
//stage 2 begin mantissa and exponent calculation
logic s2_exponent_is_even;

assign s2_exponent_is_even = ~s2_in.exponent[0];

logic[7:0] s3_exponent; 
logic s3_exponent_is_even;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_exponent_is_even <= 0;
        s3_exponent <= 0;
    end else begin
        s3_exponent_is_even <= s2_exponent_is_even;
        s3_exponent <= s2_in.exponent;
    end
end

//stage 3 new exponent calculation
logic[7:0] s3_new_exponent;
always_comb begin
    if(s3_exponent_is_even) begin
        s3_new_exponent = (8'd126 + s3_exponent) >> 1;
    end else begin
        s3_new_exponent = (8'd127 + s3_exponent) >> 1;
    end
end
logic[7:0] s4_s19_new_exponents[16];
logic s4_s17_exponent_is_even[14];

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        for(int i = 0; i < 16; i++) begin
            s4_s19_new_exponents[i] <= 0;
        end
        for(int i = 0; i < 14; i++) begin
            s4_s17_exponent_is_even[i] <= 0;
        end
    end else begin
        s4_s17_exponent_is_even[0] <= s3_exponent_is_even;
        s4_s19_new_exponents[0] <= s3_new_exponent;
        for(int i = 0; i < 15; i++) begin
            s4_s19_new_exponents[i+1] <= s4_s19_new_exponents[i];
        end
        for(int i = 0; i < 13; i++) begin
            s4_s17_exponent_is_even[i+1] <= s4_s17_exponent_is_even[i];
        end
    end
end

//stage 17, inverse square root out
//Q4.52
logic[55:0] s17_reciprocal_out;
//Q2.26
logic[27:0] s17_reciprocal_out_truncated, s17_in;

logic s17_valid_data_out;
logic[2:0] s17_rounding_mode;
fp_32b_t s17_special_result;
logic s17_input_is_invalid;
logic s17_input_is_flushed;
logic s17_special_case;
logic s17_sign;


mantissa_inverse_sqrt s17_inverse_square_root(.clk(clk), .rst(rst), .valid_data_in(s2_valid_data_in), .in(s2_in.mantissa), .rounding_mode(s2_rounding_mode), .sign(s2_in.sign),
.special_result(s2_special_result), .input_is_invalid(s2_input_is_invalid), .input_is_flushed(s2_input_is_flushed), .special_case(s2_special_case), .exponent_is_even(s2_exponent_is_even), .out(s17_reciprocal_out),
.valid_data_out(s17_valid_data_out), .rounding_mode_out(s17_rounding_mode), .special_result_out(s17_special_result), .input_is_invalid_out(s17_input_is_invalid), 
.input_is_flushed_out(s17_input_is_flushed), .special_case_out(s17_special_case), .sign_out(s17_sign));


assign s17_in = s4_s17_exponent_is_even[13] ? {1'b1, s17_special_result.mantissa, 4'b0000} : {2'b01, s17_special_result.mantissa, 3'b000};
assign s17_reciprocal_out_truncated = s17_reciprocal_out[53:26];


logic s18_valid_data_out;
logic[2:0] s18_rounding_mode;
fp_32b_t s18_special_result;
logic s18_input_is_invalid;
logic s18_input_is_flushed;
logic s18_special_case;
logic s18_sign;

logic s19_valid_data_out;
logic[2:0] s19_rounding_mode;
fp_32b_t s19_special_result;
logic s19_input_is_invalid;
logic s19_input_is_flushed;
logic s19_special_case;
logic s19_sign;

always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s18_valid_data_out <= 0;
        s18_rounding_mode <= 0;
        s18_special_result <= 0;
        s18_input_is_invalid <= 0;
        s18_input_is_flushed <= 0;
        s18_special_case <= 0;
        s18_sign <= 0;

        s19_valid_data_out <= 0;
        s19_rounding_mode <= 0;
        s19_special_result <= 0;
        s19_input_is_invalid <= 0;
        s19_input_is_flushed <= 0;
        s19_special_case <= 0;
        s19_sign <= 0;
    end else begin
        s18_valid_data_out <= s17_valid_data_out;
        s18_rounding_mode <= s17_rounding_mode;
        s18_special_result <= s17_special_result;
        s18_input_is_invalid <= s17_input_is_invalid;
        s18_input_is_flushed <= s17_input_is_flushed;
        s18_special_case <= s17_special_case;
        s18_sign <= s17_sign;

        s19_valid_data_out <= s18_valid_data_out;
        s19_rounding_mode <= s18_rounding_mode;
        s19_special_result <= s18_special_result;
        s19_input_is_invalid <= s18_input_is_invalid;
        s19_input_is_flushed <= s18_input_is_flushed;
        s19_special_case <= s18_special_case;
        s19_sign <= s18_sign;
    end
end

//q4.52
logic[55:0] s19_mult_out;
multiplier_delayed #(.WIDTH(28)) s17_mult(.clk(clk), .rst(rst), .in1(s17_in), .in2(s17_reciprocal_out_truncated), .out(s19_mult_out));
//output is in (1,2]
logic[22:0] s19_mantissa_pre_round;
logic s19_guard, s19_round, s19_sticky;
always_comb begin
    s19_mantissa_pre_round = s19_mult_out[51:29];
    s19_guard = s19_mult_out[28];
    s19_round = s19_mult_out[27];
    s19_sticky = | s19_mult_out[26:0];
end

logic s19_exponent_overflow, s19_exponent_underflow, s19_has_grs_bits;
logic[23:0] s19_rounded_mantissa_temp;
logic[22:0] s19_rounded_mantissa;
logic[7:0] s19_exponent;
logic[8:0] s19_rounded_exponent;
assign s19_exponent = s4_s19_new_exponents[15];
floating_point_rounder rounder(.mantissa(s19_mantissa_pre_round), .guard(s19_guard), .round(s19_round), .sticky(s19_sticky),
.sign(s19_sign), .rounding_mode(s19_rounding_mode), .rounded_mantissa_pre_overflow_detection(s19_rounded_mantissa_temp));

always_comb begin
    if(s19_rounded_mantissa_temp[23]) begin
        s19_rounded_mantissa = 23'd0;
        s19_rounded_exponent = {1'b0,s19_exponent} + 9'd1;
    end else begin
        s19_rounded_mantissa = s19_rounded_mantissa_temp[22:0];
        s19_rounded_exponent = {1'b0,s19_exponent};
    end


    s19_exponent_overflow = (s19_rounded_exponent > 9'd254);
    s19_exponent_underflow = (s19_rounded_exponent == 9'd0);
    s19_has_grs_bits = s19_guard | s19_round | s19_sticky;
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
        invalid_operation <= s19_input_is_invalid;
        valid_data_out <= s19_valid_data_out;
        if(s19_special_case) begin
            overflow <= 1'b0;
            inexact <= 1'b0;
            underflow <= s19_input_is_flushed;
            out <= s19_special_result;
        end else if(s19_exponent_overflow) begin
            overflow <= 1'b1;
            underflow <= 1'b0;
            inexact <= 1'b1;
            case(s19_rounding_mode)
                RTZ: begin
                    out <= {s19_sign, 8'hFE, 23'h7FFFFF};
                end
                RDN: begin
                    if(s19_sign) begin
                        out <= {1'b1, 8'hFF, 23'h0};
                    end else begin
                        out <= {1'b0, 8'hFE, 23'h7FFFFF};
                    end
                end
                RUP: begin
                    if(s19_sign) begin
                        out <= {1'b1, 8'hFE, 23'h7FFFFF};
                    end else begin
                        out <= {1'b0, 8'hFF, 23'h0};
                    end
                end
                default: begin
                    out <= {s19_sign, 8'hFF, 23'h0};
                end
            endcase
        end else if(s19_exponent_underflow) begin
            overflow <= 1'b0;
            underflow <= s19_has_grs_bits;
            inexact <= 1'b1;
            out <= {s19_sign, 8'h0, 23'h0};
        end else begin
            overflow <= 1'b0;
            underflow <= 1'b0;
            inexact <= s19_has_grs_bits;
            out <= {s19_sign, s19_rounded_exponent[7:0],  s19_rounded_mantissa};
        end
    end
end
endmodule