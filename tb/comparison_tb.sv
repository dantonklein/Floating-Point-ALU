import fp_pkg::*;

`timescale 1 ns/ 10 ps 

//This class contains the transation object for the floating point adder/sub.
//This will later be expanded for all the floating point operations

class fp_item;
    rand bit [31:0] in1_bits, in2_bits;
    rand bit [2:0] rounding_mode;
    rand bit [1:0] special_case;
    //special cases: 0 is both normal, 1 is in1 special, 2 is in2 special, 3 is both special

    constraint special_case_dist {
        special_case dist {
            2'd0 := 80, 
            2'd1 := 8,
            2'd2 := 8,
            2'd3 := 4
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
        if(special_case[0]) in1_bits = get_random_special();
        if(special_case[1]) in2_bits = get_random_special();
    endfunction
endclass

module comparison_tb #(
    parameter int NUM_TESTS = 500
);

    logic clk, rst, valid_data_in;
    logic [31:0] in1, in2;
    logic out;
    logic invalid_operation;
    logic valid_data_out;
    localparam logic[2:0] operation = OP_EQ;
    
    fp_compare #(.OPERATION(operation)) DUT (.*);

    
    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end

    fp_item item;
    //int passed = 0;
    //int failed = 0;
    int expected_out;
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
            in1 <= item.in1_bits;
            in2 <= item.in2_bits;
            valid_data_in <= 1;
            case(operation)
                OP_EQ: expected_out = $bitstoshortreal(item.in1_bits) == $bitstoshortreal(item.in2_bits);
                OP_NE: expected_out = $bitstoshortreal(item.in1_bits) != $bitstoshortreal(item.in2_bits);
                OP_LT: expected_out = $bitstoshortreal(item.in1_bits) < $bitstoshortreal(item.in2_bits);
                OP_LE: expected_out = $bitstoshortreal(item.in1_bits) <= $bitstoshortreal(item.in2_bits);
                OP_GT: expected_out = $bitstoshortreal(item.in1_bits) > $bitstoshortreal(item.in2_bits);
                OP_GE: expected_out = $bitstoshortreal(item.in1_bits) >= $bitstoshortreal(item.in2_bits);
            endcase
            
            @(posedge clk);
            $display("[%0t] Test %0d: in1=0x%08h in1float=%f in2=0x%08h in2float=%f", $time, i, in1, $bitstoshortreal(in1),in2, $bitstoshortreal(in2));
            valid_data_in <= 0;
            @(posedge valid_data_out);
            $display("Result: 0x%d [inv=%d] Expected: 0x%d\n",out, invalid_operation, expected_out);
        end

        $display("Tests completed.");
        disable generate_clock;
    end
endmodule