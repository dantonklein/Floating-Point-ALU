import fp_pkg::*;

module fp_addsub_pipeline (
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    input logic valid_data_in,
    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation,
    output logic valid_data_out
);

fp_32b_t in1_init;
fp_32b_t in2_init;
assign in1_init = in1;
assign in2_init = in2;

//flush to zero logic and other special case handling
logic in1_iszero, in2_iszero;
logic in1_isinfinite, in2_isinfinite;


endmodule