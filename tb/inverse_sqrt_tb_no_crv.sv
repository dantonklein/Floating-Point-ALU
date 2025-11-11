import fp_pkg::*;

`timescale 1 ns/ 10 ps 


module inverse_sqrt_tb_no_crv #(
);

    logic clk, rst, valid_data_in;
    logic [31:0] in;
    logic [2:0] rounding_mode;
    logic [31:0] out;
    logic overflow, underflow, inexact, invalid_operation, division_by_zero;
    logic valid_data_out;
   // logic normalized_mantissa_lsb, normalized_guard, normalized_round, normalized_sticky, round_up;

    fp_inverse_sqrt_pipeline DUT (.*);

    
    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end

    //int passed = 0;
    //int failed = 0;
    shortreal expected_out;
    initial begin
        $timeformat(-9, 0, " ns");
        rst <= 1;
        valid_data_in <= 0;
        @(posedge clk);
        rst <= 0;
        @(posedge clk);
        in = {32'h3C48208E};
        //rounding_mode <= item.rounding_mode;
        rounding_mode <= RNE;
        valid_data_in <= 1;
        expected_out = 1.0 / $sqrt($bitstoshortreal(in));
        @(posedge clk);
        $display("[%0t] in=0x%08h infloat=%f rmode=%0d", $time, in, $bitstoshortreal(in), rounding_mode);
        valid_data_in <= 0;
        @(posedge valid_data_out);
        $display("  Result: 0x%08h Float: %f [ovf=%b unf=%b inx=%b inv=%b]", out, $bitstoshortreal(out), overflow, underflow, inexact, invalid_operation);
        $display("Expected: 0x%08h Float: %f", $shortrealtobits(expected_out), expected_out);
        

        $display("Tests completed.");
        disable generate_clock;
    end
endmodule