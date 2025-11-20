import fp_pkg::*;

//these min and max modules follow the ieee 754-2019 format, where nans are propagated


module fp_min #(
    parameter logic magnitude = 0
)(
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2,
    output logic[31:0] out,
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
logic nan, both_zero;
assign nan = in1_isnan | in2_isnan;
assign both_zero = in1_iszero & in2_iszero;

logic comparison; //1 = propagate in1, 0 = propagate in2
generate 
    if(magnitude) begin : absolute_min
        assign comparison = in1[30:0] < in2[30:0];
    end else begin : signed_comparison
        always_comb begin
            case({in2_init.sign, in1_init.sign})
                2'b00: begin
                    comparison = in1[30:0] < in2[30:0];
                end
                2'b01: begin
                    comparison = 1;
                end
                2'b10: begin
                    comparison = 0;
                end
                2'b11: begin
                    comparison = in1[30:0] > in2[30:0];
                end
            endcase
        end
    end
endgenerate

logic[31:0] result;
always_comb begin
    if (in1_issnan) begin
        // Convert sNaN to qNaN by setting bit 22
        result = {in1_init.sign, in1_init.exponent, 1'b1, in1_init.mantissa[21:0]};
    end else if (in2_issnan) begin
        result = {in2_init.sign, in2_init.exponent, 1'b1, in2_init.mantissa[21:0]};
    end else if (in1_isnan) begin
        result = in1_init;  // Propagate first qNaN as-is
    end else if (in2_isnan) begin
        result = in2_init;  // Propagate second qNaN
    end
    else if(both_zero) result = in1_init.sign ? in1_init : in2_init;
    else result = comparison ? in1_init : in2_init;
end

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

module fp_max #(
    parameter logic magnitude = 0
)(
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2,
    output logic[31:0] out,
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
logic nan, both_zero;
assign nan = in1_isnan | in2_isnan;
assign both_zero = in1_iszero & in2_iszero;

logic comparison; //1 = propagate in1, 0 = propagate in2
generate 
    if(magnitude) begin : absolute_min
        assign comparison = in1[30:0] > in2[30:0];
    end else begin : signed_comparison
        always_comb begin
            case({in2_init.sign, in1_init.sign})
                2'b00: begin
                    comparison = in1[30:0] > in2[30:0];
                end
                2'b01: begin
                    comparison = 0;
                end
                2'b10: begin
                    comparison = 1;
                end
                2'b11: begin
                    comparison = in1[30:0] < in2[30:0];
                end
            endcase
        end
    end
endgenerate

logic[31:0] result;
always_comb begin
    if (in1_issnan) begin
        // Convert sNaN to qNaN by setting bit 22
        result = {in1_init.sign, in1_init.exponent, 1'b1, in1_init.mantissa[21:0]};
    end else if (in2_issnan) begin
        result = {in2_init.sign, in2_init.exponent, 1'b1, in2_init.mantissa[21:0]};
    end else if (in1_isnan) begin
        result = in1_init;  // Propagate first qNaN as-is
    end else if (in2_isnan) begin
        result = in2_init;  // Propagate second qNaN
    end
    else if(both_zero) result = in1_init.sign ? in2_init : in1_init;
    else result = comparison ? in1_init : in2_init;
end

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