import fp_pkg::*;

`timescale 1 ns/ 10 ps 



module reciprocal_no_crv_tb #(
    parameter int NUM_TESTS = 1
);

    logic clk, rst, valid_data_in;
    logic [31:0] in;
    logic [2:0] rounding_mode;
    logic [31:0] out;
    logic overflow, underflow, inexact, invalid_operation, division_by_zero;
    logic valid_data_out;
   // logic normalized_mantissa_lsb, normalized_guard, normalized_round, normalized_sticky, round_up;

    fp_reciprocal_pipeline DUT (.*);

    
    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end

    //fp_item item;
    //int passed = 0;
    //int failed = 0;
    shortreal expected_out;
    initial begin
        //item = new;
        $timeformat(-9, 0, " ns");
        rst <= 1;
        valid_data_in <= 0;
        @(posedge clk);
        rst <= 0;
        @(posedge clk);

        for(int i = 0; i < NUM_TESTS; i++) begin
            //assert(item.randomize()) 
            //else $fatal(1, "ERROR: Randomization failed.");
            @(posedge clk);
            in <= {1'b0, 8'b01111111, 23'd0};
            //rounding_mode <= item.rounding_mode;
            rounding_mode <= RNE;
            valid_data_in <= 1;
            expected_out = $bitstoshortreal({1'b0, 8'b01111111, 23'd0}) / $bitstoshortreal({1'b0, 8'b01111111, 23'd0});
            @(posedge clk);
            $display("[%0t] Test %0d: in=0x%08h infloat=%f rmode=%0d", $time, i, in, $bitstoshortreal(in), rounding_mode);
            valid_data_in <= 0;
            @(posedge valid_data_out);
            $display("  Result: 0x%08h Float: %f [ovf=%b unf=%b inx=%b inv=%b dv0=%b]", out, $bitstoshortreal(out), overflow, underflow, inexact, invalid_operation, division_by_zero);
            $display("Expected: 0x%08h Float: %f", $shortrealtobits(expected_out), expected_out);
            //$display("Normalized_Mantissa_LSB: %b Normalized_Guard: %b Normalized_Round: %b Normalized_Sticky: %b Round_Up: %b\n", normalized_mantissa_lsb, normalized_guard, normalized_round, normalized_sticky, round_up);
        end

        $display("Tests completed.");
        disable generate_clock;
    end
endmodule