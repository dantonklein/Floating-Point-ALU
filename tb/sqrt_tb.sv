import fp_pkg::*;

`timescale 1 ns/ 10 ps 

//This class contains the transation object for the floating point adder/sub.
//This will later be expanded for all the floating point operations

class fp_item;
    rand bit [31:0] in_bits;
    rand bit [2:0] rounding_mode;
    rand bit special_case;

    constraint special_case_dist {
        special_case dist {
            1'b0 := 96, 
            1'b1 := 4
        };
    }

    constraint rounding_mode_dist {
        rounding_mode dist {
            RNE := 60,
            RTZ := 10,
            RDN := 10,
            RUP := 10,
            RMM := 10
        };
    }

    //generate special value function
    function bit [31:0] get_random_special();
        bit[31:0] result;
        bit[3:0] special_type = $urandom_range(0,11);

        case(special_type)
            4'd0: result = 32'h00000000; //positive zero
            4'd1: result = 32'h80000000; //negative zero
            4'd2: result = 32'h7F800000; //positive infinity
            4'd3: result = 32'hFF800000; //negative infinity
            4'd4: result = 32'h7FC00000 | ($urandom() & 32'h003FFFFF); //QNaN
            4'd5: begin //SNaN
                result = 32'h7F800000 | $urandom_range(1, 32'h001FFFFF);
            end
            4'd6: begin //positive denormalized number
                result = $urandom_range(1, 32'h007FFFFF);
            end
            4'd7: begin //negative denormalized number
                result = 32'h80000000 | $urandom_range(1, 32'h007FFFFF);
            end
            4'd8: result = 32'h00800000 | ($urandom() & 32'h007FFFFF); // small positive number
            4'd9: result = 32'h80800000 | ($urandom() & 32'h007FFFFF); // small negative number
            4'd10: result = 32'h7F000000 | ($urandom() & 32'h007FFFFF); // big positive number
            4'd11: result = 32'hFF000000 | ($urandom() & 32'h007FFFFF); // big negative number
        endcase

        return result;
    endfunction

    function void post_randomize();
        if(special_case) in_bits = get_random_special();
    endfunction
endclass

module sqrt_tb #(
    parameter int NUM_TESTS = 100
);

    logic clk, rst, valid_data_in;
    logic [31:0] in;
    logic [2:0] rounding_mode;
    logic [31:0] out;
    logic overflow, underflow, inexact, invalid_operation;
    logic valid_data_out;
   // logic normalized_mantissa_lsb, normalized_guard, normalized_round, normalized_sticky, round_up;

    fp_sqrt_pipeline DUT (.*);

    
    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end

    fp_item item;
    //int passed = 0;
    //int failed = 0;
    shortreal expected_out;
    initial begin
        item = new;
        $timeformat(-9, 0, " ns");
        rst <= 1;
        valid_data_in <= 0;
        @(posedge clk);
        rst <= 0;
        @(posedge clk);

        for(int i = 0; i < NUM_TESTS; i++) begin
            assert(item.randomize()) 
            else $fatal(1, "ERROR: Randomization failed.");
            @(posedge clk);
            if(i == 50) begin
                in = {1'b0, 8'b01111111, 23'd0};
            end else if(i == 51) begin
                in = {1'b0, 8'b10000000, 23'd0};
            end else begin
                in = {1'b0, item.in_bits[30:0]};
            end
            //rounding_mode <= item.rounding_mode;
            rounding_mode <= RNE;
            valid_data_in <= 1;
            expected_out = $sqrt($bitstoshortreal(in));
            @(posedge clk);
            $display("[%0t] Test %0d: in=0x%08h infloat=%f rmode=%0d", $time, i, in, $bitstoshortreal(in), rounding_mode);
            valid_data_in <= 0;
            @(posedge valid_data_out);
            $display("  Result: 0x%08h Float: %f [ovf=%b unf=%b inx=%b inv=%b]", out, $bitstoshortreal(out), overflow, underflow, inexact, invalid_operation);
            $display("Expected: 0x%08h Float: %f", $shortrealtobits(expected_out), expected_out);
            //$display("Normalized_Mantissa_LSB: %b Normalized_Guard: %b Normalized_Round: %b Normalized_Sticky: %b Round_Up: %b\n", normalized_mantissa_lsb, normalized_guard, normalized_round, normalized_sticky, round_up);
        end

        $display("Tests completed.");
        disable generate_clock;
    end
endmodule