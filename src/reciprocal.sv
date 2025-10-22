module mantissa_reciprocal_24bit_LUT (
    input logic clk,
    input logic[7:0] in,
    output logic[23:0] out
);
    logic[23:0] lut[256];

    initial begin
        $readmemh("reciprocal_lut256.mem", lut);
    end

    always_ff @(posedge clk) begin
        out <= lut[in];
    end
endmodule

module mantissa_reciprocal_24bit (
    input logic clk, rst, valid_data_in,
    input logic[23:0] in,
    output logic[23:0] out,
    output logic valid_data_out
);

endmodule