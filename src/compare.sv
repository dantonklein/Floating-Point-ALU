import fp_pkg::*;

module fp_compare #(
    parameter logic[2:0] operation = OP_EQ
)(
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    output logic out,
    output logic invalid_operation,
    output logic valid_data_out
);
fp_32b_t in1_init;
fp_32b_t in2_init;
assign in1_init = in1;
assign in2_init = in2;

logic in1_iszero, in2_iszero;
logic in1_isnan, in2_isnan;
logic in1_issnan, in2_issnan;


//special cases for input
always_comb begin
    //input 1
    in1_iszero = (in1_init.exponent == '0) & (in1_init.mantissa == '0);
    in1_isnan = (in1_init.exponent == '1) & (in1_init.mantissa != '0);
    in1_issnan = in1_isnan & ~in1_init.mantissa[22];

    in2_iszero = (in2_init.exponent == '0) & (in2_init.mantissa == '0);
    in2_isnan = (in2_init.exponent == '1) & (in2_init.mantissa != '0);
    in2_issnan =  in2_isnan & ~in2_init.mantissa[22];
    
end
logic operation_result;
logic nan, both_zero;
assign nan = in1_isnan | in2_isnan;
assign both_zero = in1_iszero & in2_iszero;

generate

if(operation == OP_EQ) begin : gen_Equal
    assign operation_result = (in1==in2) | both_zero;
end else if(operation == OP_NE) begin : gen_Not_Equal
    assign operation_result = in1!=in2 & ~both_zero;
end else if(operation == OP_LT) begin : gen_Less_Than
    logic less_than;
    always_comb begin
        case({in2_init.sign, in1_init.sign})
            2'b00: begin
                less_than = in1[30:0] < in2[30:0];
            end
            2'b01: begin
                less_than = 1;
            end
            2'b10: begin
                less_than = 0;
            end
            2'b11: begin
                less_than = in1[30:0] > in2[30:0];
            end
        endcase
        operation_result = less_than & ~both_zero;
    end
end else if(operation == OP_LE) begin : gen_Less_Than_Or_Equal_To
    logic less_than_or_equal_to;
    always_comb begin
        case({in2_init.sign, in1_init.sign})
            2'b00: begin
                less_than_or_equal_to = in1[30:0] <= in2[30:0];
            end
            2'b01: begin
                less_than_or_equal_to = 1;
            end
            2'b10: begin
                less_than_or_equal_to = 0;
            end
            2'b11: begin
                less_than_or_equal_to = in1[30:0] >= in2[30:0];
            end
        endcase
        operation_result = less_than_or_equal_to | both_zero;
    end
end else if(operation == OP_GT) begin : gen_Greater_Than
    logic greater_than;
    always_comb begin
        case({in2_init.sign, in1_init.sign})
            2'b00: begin
                greater_than = in1[30:0] > in2[30:0];
            end
            2'b01: begin
                greater_than = 0;
            end
            2'b10: begin
                greater_than = 1;
            end
            2'b11: begin
                greater_than = in1[30:0] < in2[30:0];
            end
        endcase
        operation_result = greater_than & ~both_zero;
    end
end else if(operation == OP_GE) begin : gen_Greater_Than_Or_Equal_To
    logic greater_than_or_equal_to;
    always_comb begin
        case({in2_init.sign, in1_init.sign})
            2'b00: begin
                greater_than_or_equal_to = in1[30:0] >= in2[30:0];
            end
            2'b01: begin
                greater_than_or_equal_to = 0;
            end
            2'b10: begin
                greater_than_or_equal_to = 1;
            end
            2'b11: begin
                greater_than_or_equal_to = in1[30:0] <= in2[30:0];
            end
        endcase
        operation_result = greater_than_or_equal_to | both_zero;
    end
end else begin : gen_no_option_specified
    assign operation_result = 0;
end

endgenerate

logic result;
generate
if(operation == OP_NE) begin
    assign result = (in1_isnan | in2_isnan) ? 1 : operation_result;
end
else begin
    assign result = (in1_isnan | in2_isnan) ? 0 : operation_result;
end
endgenerate

    always_ff @(posedge clk or posedge rst) begin
        if(rst) begin
            out <= 0;
            invalid_operation <= 0;
            valid_data_out <= 0;
        end else begin
            out <= result;
            invalid_operation <= in1_issnan | in2_issnan;
            valid_data_out <= valid_data_in;
        end
    end
endmodule