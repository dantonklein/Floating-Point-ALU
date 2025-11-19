import fp_pkg::*;

`timescale 1 ns/ 10 ps 


module comparison_tb_no_crv #(
    parameter int NUM_TESTS = 500
);

    logic clk, rst, valid_data_in;
    logic [31:0] in1, in2;
    logic out;
    logic invalid_operation;
    logic valid_data_out;
    localparam logic[2:0] operation = OP_NE;
    
    fp_compare #(.OPERATION(operation)) DUT (.*);

    
    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end

    int expected_out;
    initial begin
        $timeformat(-9, 0, " ns");
        rst <= 1;
        valid_data_in <= 0;
        @(posedge clk);
        rst <= 0;

        @(posedge clk);
        in1 = {1'b0, 8'd253, 23'b1};
        in2 = {1'b0, 8'd255, 23'b1};
        valid_data_in <= 1;
        case(operation)
            OP_EQ: expected_out = $bitstoshortreal(in1) == $bitstoshortreal(in2);
            OP_NE: expected_out = $bitstoshortreal(in1) != $bitstoshortreal(in2);
            OP_LT: expected_out = $bitstoshortreal(in1) < $bitstoshortreal(in2);
            OP_LE: expected_out = $bitstoshortreal(in1) <= $bitstoshortreal(in2);
            OP_GT: expected_out = $bitstoshortreal(in1) > $bitstoshortreal(in2);
            OP_GE: expected_out = $bitstoshortreal(in1) >= $bitstoshortreal(in2);
        endcase
        
        @(posedge clk);
        $display("[%0t] in1=0x%08h in1float=%f in2=0x%08h in2float=%f", $time, in1, $bitstoshortreal(in1),in2, $bitstoshortreal(in2));
        valid_data_in <= 0;
        @(posedge valid_data_out);
        $display("Result: 0x%d [inv=%d] Expected: 0x%d\n",out, invalid_operation, expected_out);

        $display("Tests completed.");
        disable generate_clock;
    end
endmodule