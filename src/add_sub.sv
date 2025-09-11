import fp_pkg::*;

module fp_addsub_pipeline (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation,
    output logic valid_data_out
);

//input flag handling
fp_32b_t in1_init;
fp_32b_t in2_init;
assign in1_init = in1;
assign in2_init = in2;

//special case handling
logic in1_iszero, in2_iszero;
logic in1_isinfinite, in2_isinfinite;
logic in1_isqnan, in2_isqnan;
logic in1_issnan, in2_issnan;
logic in1_isdenorm, in2_isdenorm;

//special cases for input
always_comb begin
    //input 1
    in1_iszero = (in1_init.exponent == '0) & (in1_init.mantissa == '0);
    in1_isinfinite = (in1_init.exponent == '1) & (in1_init.mantissa == '0);
    in1_isqnan = (in1_init.exponent == '1) & (in1_init.mantissa != '0) & in1_init.mantissa[22];
    in1_issnan = (in1_init.exponent == '1) & (in1_init.mantissa != '0) & ~in1_init.mantissa[22];
    in1_isdenorm = (in1_init.exponent == '0) & (in1_init.mantissa != '0);

    in2_iszero = (in2_init.exponent == '0) & (in2_init.mantissa == '0);
    in2_isinfinite = (in2_init.exponent == '1) & (in2_init.mantissa == '0);
    in2_isqnan = (in2_init.exponent == '1) & (in2_init.mantissa != '0) & in2_init.mantissa[22];
    in2_issnan = (in2_init.exponent == '1) & (in2_init.mantissa != '0) & ~in2_init.mantissa[22];
    in2_isdenorm = (in2_init.exponent == '0) & (in2_init.mantissa != '0);
end
//flush to zero and invalid input handling
logic input_is_invalid;
logic input_is_flushed;
assign input_is_invalid = in1_issnan | in2_issnan | (in1_isinfinite & in2_isinfinite & (in1_isinfinite ^ in2_isinfinite));
assign input_is_flushed = in1_isdenorm | in2_isdenorm;

//Stage 1: Denorm, NaN processing
fp_32b_t s1_special_result, s1_in1, s1_in2;
logic s1_input_is_invalid;
logic s1_input_is_flushed;
logic s1_special_case;
logic s1_valid_data_in;
logic[2:0] s1_rounding_mode;


always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s1_in1 <= '0;
        s1_in2 <= '0;
        s1_input_is_invalid <= 0;
        s1_input_is_flushed <= 0;
        s1_valid_data_in <= 0;
        s1_rounding_mode <= 0;
        s1_special_case <= 0;
        s1_special_result <= '0;
    end else begin
        s1_in1 <= in1_init;
        s1_in2 <= in2_init;
        s1_input_is_invalid <= input_is_invalid;
        s1_input_is_flushed <= input_is_flushed;
        s1_valid_data_in <= valid_data_in;
        s1_rounding_mode <= rounding_mode;


        //Special cases:

        //propagate qnan
        if(in1_isqnan) begin
            s1_special_case <= 1;
            s1_special_result <= in1_init;

        end else if(in2_isqnan) begin
            s1_special_case <= 1;
            s1_special_result <= in2_init;

        //convert snan to qnan
        end else if(in1_issnan) begin
            s1_special_case <= 1;
            s1_special_result <= in1_init | 32'b00000000010000000000000000000000;

        end else if(in2_issnan) begin
            s1_special_case <= 1;
            s1_special_result <= in2_init | 32'b00000000010000000000000000000000;

        //when both infinity
        end else if(in1_isinfinite & in2_isinfinite) begin
            s1_special_case <= 1;
            //same sign means that infinity gets propagated
            if(in1_init.sign == in2_init.sign) begin
                s1_special_result <= in1_init;
            //infinity - infinity = snan, converted to qnan but triggers the invalid operation
            end else begin
                s1_special_result <= 32'h7FC00000;
            end

        //infinite input + non-infinite, infinity gets propagated
        end else if(in1_isinfinite) begin
            s1_special_case <= 1;
            s1_special_result <= in1_init;
        end else if(in2_isinfinite) begin
            s1_special_case <= 1;
            s1_special_result <= in2_init;

        //if both zeroes, different cases
        end else if((in1_iszero | in1_isdenorm) & (in2_iszero | in2_isdenorm)) begin
            s1_special_case <= 1;
        //both positive zero -> positive zero
            if(in1_init.sign & in2_init.sign) begin
                s1_special_result <= in1_init;
        //both negative zero -> negative zero
            end else if(~in1_init.sign & ~in2_init.sign) begin
                s1_special_result <= in1_init;
        //round down(RD) is a special case for when the signs differ
            end else begin
                if(rounding_mode == RDN) begin
                    s1_special_result <= 32'h80000000;
                end else begin
                    s1_special_result <= 32'h00000000;
                end
            end
        //any input is zero or denormal, propagate the other 
        end else if(in1_iszero | in1_isdenorm) begin
            s1_special_case <= 1;
            if(in2_isdenorm) s1_special_result <= {in2_init.sign, 31'd0};
            else s1_special_result <= in2_init;

        end else if(in2_iszero | in2_isdenorm) begin
            s1_special_case <= 1;
            if(in1_isdenorm) s1_special_result <= {in1_init.sign, 31'd0};
            else s1_special_result <= in1_init;
        end else begin
            s1_special_case <= 0;
            //this doesnt matter
            s1_special_result <= '0;
        end
    end
end


endmodule