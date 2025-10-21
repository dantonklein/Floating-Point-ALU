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
            s2_special_case <= 1;
            s2_special_result <= {s1_in1_init.sign ^ s1_in2_init.sign,31'h7FC00000};

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

logic signed[9:0] s2_exponent_add; //extra bit to account for overflow
assign s2_exponent_add = $signed({1'b0, s2_in1.exponent}) + $signed({1'b0, s2_in2.exponent}) - 10'sd127;

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

logic signed[9:0] s3_exponent_add;
logic s3_sign_bit;

fp_32b_t s4_special_result;
logic s4_input_is_invalid;
logic s4_input_is_flushed;
logic s4_special_case;
logic[2:0] s4_rounding_mode;

logic s4_sign_bit;
logic signed[9:0] s4_exponent_add;
logic[47:0] s4_multiplier_out;
logic s4_valid_data_in;


always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s3_special_result <= 0;
        s3_input_is_invalid <= 0;
        s3_input_is_flushed <= 0;
        s3_special_case <= 0;
        s3_rounding_mode <= 0;

        s3_exponent_add <= 0;
        s3_sign_bit <= 0;

        s4_special_result <= 0;
        s4_input_is_invalid <= 0;
        s4_input_is_flushed <= 0;
        s4_special_case <= 0;
        s4_rounding_mode <= 0;

        s4_sign_bit <= 0;
        s4_exponent_add <= 0;
    end else begin
        s3_special_result <= s2_special_result;
        s3_input_is_invalid <= s2_input_is_invalid;
        s3_input_is_flushed <= s2_input_is_flushed;
        s3_special_case <= s2_special_case;
        s3_rounding_mode <= rounding_mode;

        s3_exponent_add <= s2_exponent_add;
        s3_sign_bit <= s2_sign_bit;

        s4_special_result <= s3_special_result;
        s4_input_is_invalid <= s3_input_is_invalid;
        s4_input_is_flushed <= s3_input_is_flushed;
        s4_special_case <= s3_special_case;
        s4_rounding_mode <= s3_rounding_mode;

        s4_exponent_add <= s3_exponent_add;
        s4_sign_bit <= s3_sign_bit;
    end
end

Dadda_Multiplier_24bit_pipelined s2_4_multiplier(.clk(clk), .rst(rst), .valid_data_in(s2_valid_data_in), 
.in1(s2_multiplier_in1), .in2(s2_multiplier_in2), .out(s4_multiplier_out), .valid_data_out(s4_valid_data_in));

//handle shifting
logic[22:0] s4_normalized_mantissa;
logic[9:0] s4_normalized_exponent;
logic s4_guard, s4_round, s4_sticky;
always_comb begin
    if(s4_multiplier_out[47]) begin
        s4_normalized_mantissa = s4_multiplier_out[46:24];
        s4_guard = s4_multiplier_out[23];
        s4_round = s4_multiplier_out[22];
        s4_sticky = | s4_multiplier_out[21:0];
        s4_normalized_exponent = s4_exponent_add + 10'sd1;
    end else begin
        s4_normalized_mantissa = s4_multiplier_out[45:23];
        s4_guard = s4_multiplier_out[22];
        s4_round = s4_multiplier_out[21];
        s4_sticky = | s4_multiplier_out[20:0];
        s4_normalized_exponent = s4_exponent_add;
    end
end

//rounding and flush to zero
logic s4_round_up;
logic s4_exponent_overflow, s4_exponent_underflow, s4_has_grs_bits;
logic[23:0] s4_rounded_mantissa_temp;
logic[22:0] s4_rounded_mantissa;
logic signed[9:0] s4_rounded_exponent;
always_comb begin
    case(s4_rounding_mode) 
        RNE: begin
            s4_round_up = s4_guard & (s4_round | s4_sticky | s4_normalized_mantissa[0]);
        end
        RTZ: begin
            s4_round_up = 1'b0;
        end
        RDN: begin
            s4_round_up = s4_sign_bit & (s4_guard | s4_round | s4_sticky);
        end
        RUP: begin
            s4_round_up = ~s4_sign_bit & (s4_guard | s4_round | s4_sticky);
        end
        RMM: begin
            s4_round_up = s4_guard;
        end
        default: begin
            s4_round_up = 1'b0;
        end
    endcase

    s4_rounded_mantissa_temp = {1'b0, s4_normalized_mantissa} + s4_round_up;

    if(s4_rounded_mantissa_temp[23]) begin
        s4_rounded_mantissa = 23'd0;
        s4_rounded_exponent = s4_normalized_exponent + 10'sd1;
    end else begin
        s4_rounded_mantissa = s4_rounded_mantissa_temp[22:0];
        s4_rounded_exponent = s4_normalized_exponent;
    end


    s4_exponent_overflow = (s4_rounded_exponent > 10'sd254);
    s4_exponent_underflow = (s4_rounded_exponent <= 10'sd0);
    s4_has_grs_bits = s4_guard | s4_round | s4_sticky;
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
        invalid_operation <= s4_input_is_invalid;
        valid_data_out <= s4_valid_data_in;
        if(s4_special_case) begin
            overflow <= 1'b0;
            inexact <= 1'b0;
            underflow <= s4_input_is_flushed;
            out <= s4_special_result;
        end else if(s4_exponent_overflow) begin
            overflow <= 1'b1;
            underflow <= 1'b0;
            inexact <= 1'b1;
            case(s4_rounding_mode)
                RTZ: begin
                    out = {s4_sign_bit, 8'hFE, 23'h7FFFFF};
                end
                RDN: begin
                    if(s4_sign_bit) begin
                        out <= {1'b1, 8'hFF, 23'h0};
                    end else begin
                        out <= {1'b0, 8'hFE, 23'h7FFFFF};
                    end
                end
                RUP: begin
                    if(s4_sign_bit) begin
                        out <= {1'b1, 8'hFE, 23'h7FFFFF};
                    end else begin
                        out <= {1'b0, 8'hFF, 23'h0};
                    end
                end
                default: begin
                    out <= {s4_sign_bit, 8'hFF, 23'h0};
                end
            endcase
        end else if(s4_exponent_underflow) begin
            overflow <= 1'b0;
            underflow <= s4_has_grs_bits;
            inexact <= 1'b1;
            if(s4_rounding_mode == RDN) begin
                out <= {1'b1, 8'h0, 23'h0};
            end else begin
                out <= {s4_sign_bit, 8'h0, 23'h0};
            end
        end else begin
            overflow <= 1'b0;
            underflow <= 1'b0;
            inexact <= s4_has_grs_bits;
            out <= {s4_sign_bit, s4_rounded_exponent[7:0],  s4_rounded_mantissa};
        end
    end
end
endmodule