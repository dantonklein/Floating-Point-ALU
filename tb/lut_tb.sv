`timescale 1 ns/ 10 ps 

module lut_tb #(
    parameter int NUM_TESTS = 256
);

    logic clk;
    logic [7:0] in;
    logic [23:0] out;
    
    mantissa_reciprocal_24bit_LUT DUT (.*);

    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end

    initial begin
        $timeformat(-9, 0, " ns");
        for(int i = 0; i < NUM_TESTS; i++) begin
            in <= i;
            repeat(2) @(posedge clk);
        end
        $display("Tests completed.");
        disable generate_clock;
    end
endmodule