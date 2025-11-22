import fp_pkg::*;
//top level entity that instantiates all the floating point operations. used for timing optimization in vivado
//the operation select doesnt make sense

//list of operations:
//0. addsub, 1. multiply, 2. divide, 3. sqrt, 4. max, 5. fused mult-add
//6. equal, 7. less or equal




module floating_point_alu (
    input logic clk, rst, valid_data_in,
    input logic[31:0] in1, in2, in3,
    input logic[2:0] rounding_mode,
    input logic[2:0] op_sel,

    output logic[31:0] out,
    output logic overflow, underflow, inexact, invalid_operation, division_by_zero,
    output logic valid_data_out
);


logic[31:0] out_addsub;
logic overflow_addsub, underflow_addsub, inexact_addsub, invalid_operation_addsub;
logic valid_data_out_addsub;

fp_addsub_pipeline addsub (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in1(in1), .in2(in2), .rounding_mode(rounding_mode),
    .out(out_addsub), .overflow(overflow_addsub), .underflow(underflow_addsub), 
    .inexact(inexact_addsub), .invalid_operation(invalid_operation_addsub),
    .valid_data_out(valid_data_out_addsub)
);

logic[31:0] out_multiply;
logic overflow_multiply, underflow_multiply, inexact_multiply, invalid_operation_multiply;
logic valid_data_out_multiply;

fp_multiply_pipeline multiply (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in1(in1), .in2(in2), .rounding_mode(rounding_mode),
    .out(out_multiply), .overflow(overflow_multiply), .underflow(underflow_multiply), 
    .inexact(inexact_multiply), .invalid_operation(invalid_operation_multiply),
    .valid_data_out(valid_data_out_multiply)
);

logic[31:0] out_division;
logic overflow_division, underflow_division, inexact_division, invalid_operation_division, division_by_zero_division;
logic valid_data_out_division;

fp_division_pipeline division (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in1(in1), .in2(in2), .rounding_mode(rounding_mode),
    .out(out_division), .overflow(overflow_division), .underflow(underflow_division), 
    .inexact(inexact_division), .invalid_operation(invalid_operation_division),
    .division_by_zero(division_by_zero_division), .valid_data_out(valid_data_out_division)
);

logic[31:0] out_sqrt;
logic overflow_sqrt, underflow_sqrt, inexact_sqrt, invalid_operation_sqrt;
logic valid_data_out_sqrt;

fp_sqrt_pipeline sqrt (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in(in1), .rounding_mode(rounding_mode),
    .out(out_sqrt), .overflow(overflow_sqrt), .underflow(underflow_sqrt), 
    .inexact(inexact_sqrt), .invalid_operation(invalid_operation_sqrt),
    .valid_data_out(valid_data_out_sqrt)
);

logic[31:0] out_max;
logic invalid_operation_max;
logic valid_data_out_max;

fp_max max (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in1(in1), .in2(in2), .out(out_max), 
    .invalid_operation(invalid_operation_max),
    .valid_data_out(valid_data_out_max)
);

logic[31:0] out_fused_mult_add;
logic overflow_fused_mult_add, underflow_fused_mult_add, inexact_fused_mult_add, invalid_operation_fused_mult_add;
logic valid_data_out_fused_mult_add;

fp_fused_mult_add_pipeline fused_mult_add (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in1(in1), .in2(in2), .in3(in3), .rounding_mode(rounding_mode),
    .out(out_fused_mult_add), .overflow(overflow_fused_mult_add), .underflow(underflow_fused_mult_add), 
    .inexact(inexact_fused_mult_add), .invalid_operation(invalid_operation_fused_mult_add),
    .valid_data_out(valid_data_out_fused_mult_add)
);

logic out_equal;
logic invalid_operation_equal;
logic valid_data_out_equal;

fp_compare #(.OPERATION(OP_EQ)) equal (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in1(in1), .in2(in2), .out(out_equal), 
    .invalid_operation(invalid_operation_equal),
    .valid_data_out(valid_data_out_equal)
);

logic out_less_or_equal;
logic invalid_operation_less_or_equal;
logic valid_data_out_less_or_equal;

fp_compare #(.OPERATION(OP_LE)) less_or_equal (
    .clk(clk), .rst(rst), .valid_data_in(valid_data_in),
    .in1(in1), .in2(in2), .out(out_less_or_equal), 
    .invalid_operation(invalid_operation_less_or_equal),
    .valid_data_out(valid_data_out_less_or_equal)
);


always_comb begin
    //out, invalid, valid_data_out handling
    case(op_sel)
        3'b000: begin
            out = out_addsub;
            invalid_operation = invalid_operation_addsub;
            valid_data_out = valid_data_out_addsub;
        end
        3'b001: begin
            out = out_multiply;
            invalid_operation = invalid_operation_multiply;
            valid_data_out = valid_data_out_multiply;
        end
        3'b010: begin
            out = out_division;
            invalid_operation = invalid_operation_division;
            valid_data_out = valid_data_out_division;
        end
        3'b011: begin
            out = out_sqrt;
            invalid_operation = invalid_operation_sqrt;
            valid_data_out = valid_data_out_sqrt;
        end
        3'b100: begin
            out = out_max;
            invalid_operation = invalid_operation_max;
            valid_data_out = valid_data_out_max;
        end
        3'b101: begin
            out = out_fused_mult_add;
            invalid_operation = invalid_operation_fused_mult_add;
            valid_data_out = valid_data_out_fused_mult_add;
        end
        3'b110: begin
            out = {31'b0, out_equal};
            invalid_operation = invalid_operation_equal;
            valid_data_out = valid_data_out_equal;
        end
        3'b111: begin
            out = {31'b0, out_less_or_equal};
            invalid_operation = invalid_operation_less_or_equal;
            valid_data_out = valid_data_out_less_or_equal;
        end
    endcase
    //overflow, underflow, inexact handling
    case(op_sel)
        3'b000: begin
            overflow = overflow_addsub;
            underflow = underflow_addsub;
            inexact = inexact_addsub;
        end
        3'b001: begin
            overflow = overflow_multiply;
            underflow = underflow_multiply;
            inexact = inexact_multiply;
        end
        3'b010: begin
            overflow = overflow_division;
            underflow = underflow_division;
            inexact = inexact_division;
        end
        3'b011: begin
            overflow = overflow_sqrt;
            underflow = underflow_sqrt;
            inexact = inexact_sqrt;
        end
        3'b101: begin
            overflow = overflow_fused_mult_add;
            underflow = underflow_fused_mult_add;
            inexact = inexact_fused_mult_add;
        end
        default: begin
            overflow = 1'b0;
            underflow = 1'b0;
            inexact = 1'b0;
        end
    endcase
    //division_by_zero handling
    if(op_sel == 3'b010) begin
        division_by_zero = division_by_zero_division;
    end
    else division_by_zero = 0;
end

endmodule