import fp_pkg::*;

`timescale 1 ns/ 10 ps 


module  max_min_tb;

    logic clk, rst, valid_data_in;
    logic [31:0] in1, in2;
    logic [31:0] out_min, out_max;
    logic invalid_operation_min, invalid_operation_max;
    logic valid_data_out_min, valid_data_out_max;
    parameter logic magnitude = 0;

    fp_min #(.magnitude(magnitude)) DUT_MIN (
        .clk(clk),
        .rst(rst),
        .valid_data_in(valid_data_in),
        .in1(in1),
        .in2(in2),
        .out(out_min),
        .invalid_operation(invalid_operation_min),
        .valid_data_out(valid_data_out_min)
    );

    fp_max #(.magnitude(magnitude)) DUT_MAX (
        .clk(clk),
        .rst(rst),
        .valid_data_in(valid_data_in),
        .in1(in1),
        .in2(in2),
        .out(out_max),
        .invalid_operation(invalid_operation_max),
        .valid_data_out(valid_data_out_max)
    );
    
    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end


    typedef struct {
        bit[31:0] in1;
        bit[31:0] in2;
        bit[31:0] expected_min;
        bit[31:0] expected_max;
        bit invalid_flag;
        string description;
    } test_case_t;

    test_case_t test_cases[] = '{
        // NaN cases
        '{32'h7FC00000, 32'h40A00000, 32'h7FC00000, 32'h7FC00000, 0, "qNaN, 5.0"},
        '{32'h40A00000, 32'h7FC00000, 32'h7FC00000, 32'h7FC00000, 0, "5.0, qNaN"},
        '{32'h7FA00000, 32'h40A00000, 32'h7FC00000, 32'h7FC00000, 1, "sNaN, 5.0"},
        '{32'h7FC00000, 32'h7FC00000, 32'h7FC00000, 32'h7FC00000, 0, "qNaN, qNaN"},
        
        // Signed zeros
        '{32'h00000000, 32'h80000000, 32'h80000000, 32'h00000000, 0, "+0, -0"},
        '{32'h80000000, 32'h00000000, 32'h80000000, 32'h00000000, 0, "-0, +0"},
        '{32'h00000000, 32'h00000000, 32'h00000000, 32'h00000000, 0, "+0, +0"},
        '{32'h80000000, 32'h80000000, 32'h80000000, 32'h80000000, 0, "-0, -0"},
        
        // Infinities
        '{32'h7F800000, 32'h7F800000, 32'h7F800000, 32'h7F800000, 0, "+inf, +inf"},
        '{32'hFF800000, 32'hFF800000, 32'hFF800000, 32'hFF800000, 0, "-inf, -inf"},
        '{32'h7F800000, 32'hFF800000, 32'hFF800000, 32'h7F800000, 0, "+inf, -inf"},
        '{32'h7F800000, 32'h40A00000, 32'h40A00000, 32'h7F800000, 0, "+inf, 5.0"},
        '{32'hFF800000, 32'h40A00000, 32'hFF800000, 32'h40A00000, 0, "-inf, 5.0"},
        
        // Infinity with zeros
        '{32'h7F800000, 32'h00000000, 32'h00000000, 32'h7F800000, 0, "+inf, +0"},
        '{32'hFF800000, 32'h80000000, 32'hFF800000, 32'h80000000, 0, "-inf, -0"},
        
        // NaN with infinity
        '{32'h7FC00000, 32'h7F800000, 32'h7FC00000, 32'h7FC00000, 0, "qNaN, +inf"},
        '{32'h7FC00000, 32'hFF800000, 32'h7FC00000, 32'h7FC00000, 0, "qNaN, -inf"},
        
        // Denormals
        '{32'h00000001, 32'h00000002, 32'h00000001, 32'h00000002, 0, "denorm, denorm"},
        '{32'h00000001, 32'h00000000, 32'h00000000, 32'h00000001, 0, "denorm, +0"},
        '{32'h80000001, 32'h80000000, 32'h80000001, 32'h80000000, 0, "-denorm, -0"},
        
        // Normal mixed cases
        '{32'h40A00000, 32'hC0A00000, 32'hC0A00000, 32'h40A00000, 0, "5.0, -5.0"},
        '{32'hC0400000, 32'hC0800000, 32'hC0800000, 32'hC0400000, 0, "-3.0, -4.0"},
        '{32'h3F800000, 32'h40000000, 32'h3F800000, 32'h40000000, 0, "1.0, 2.0"}
    };
    //int passed = 0;
    //int failed = 0;
    initial begin
        $timeformat(-9, 0, " ns");
        rst <= 1;
        valid_data_in <= 0;
        in1 = 0;
        in2 = 0;
        @(posedge clk);
        rst <= 0;
        @(posedge clk);

        foreach(test_cases[i]) begin
            @(posedge clk);
            in1 <= test_cases[i].in1;
            in2 <= test_cases[i].in2;
            valid_data_in <= 1;
            @(posedge clk);
            $display("[%0t] Test %0d: %s", $time, i, test_cases[i].description);
            valid_data_in <= 0;
            @(posedge valid_data_out_min);
            $display("Min Result: 0x%08h [inv=%d] Expected: 0x%08h [inv=%d]",out_min, invalid_operation_min, test_cases[i].expected_min, test_cases[i].invalid_flag);
            $display("Max Result: 0x%08h [inv=%d] Expected: 0x%08h [inv=%d]\n\n",out_max, invalid_operation_max, test_cases[i].expected_max, test_cases[i].invalid_flag);
        end

        $display("Tests completed.");
        disable generate_clock;
    end
endmodule