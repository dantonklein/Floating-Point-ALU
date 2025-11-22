import fp_pkg::*;

`timescale 1 ns/ 10 ps 

module add_sub_tb_no_crv;

    logic clk, rst, valid_data_in;
    logic [31:0] in1, in2;
    logic [31:0] out;
    logic overflow, underflow, inexact, invalid_operation;
    logic valid_data_out;
    logic [2:0] rounding_mode;
    logic guard, round, sticky;
    logic [22:0] normalized_mantissa;

    fp_addsub_pipeline DUT (
        .clk(clk),
        .rst(rst),
        .valid_data_in(valid_data_in),
        .in1(in1),
        .in2(in2),
        .rounding_mode(rounding_mode),
        .out(out),
        .overflow(overflow),
        .underflow(underflow),
        .inexact(inexact),
        .invalid_operation(invalid_operation),
        .valid_data_out(valid_data_out),
        .guard(guard),
        .round(round),
        .sticky(sticky),
        .normalized_mantissa(normalized_mantissa)
    );
    
    initial begin : generate_clock
        clk = 1'b0;
        forever #5 clk <= ~clk;
    end

    typedef struct {
        bit[31:0] in1;
        bit[31:0] in2;
        bit[2:0] rmode;
        bit exp_overflow;
        bit exp_underflow;
        bit exp_inexact;
        bit exp_invalid;
        string description;
    } test_case_t;

    test_case_t test_cases[] = '{
        // ===== ADDITION TESTS (same sign) =====
        
        // // Basic addition
        // '{32'h3F800000, 32'h3F800000, RNE, 0, 0, 0, 0, "ADD: 1.0 + 1.0 = 2.0"},
        // '{32'h40000000, 32'h40400000, RNE, 0, 0, 0, 0, "ADD: 2.0 + 3.0 = 5.0"},
        // '{32'h40A00000, 32'h41200000, RNE, 0, 0, 0, 0, "ADD: 5.0 + 10.0 = 15.0"},
        
        // // ===== SUBTRACTION TESTS (opposite sign -> effective subtraction) =====
        
        // // Basic subtraction via opposite signs
        // '{32'h40A00000, 32'hC0400000, RNE, 0, 0, 0, 0, "SUB: 5.0 + (-3.0) = 2.0"},
        // '{32'h40400000, 32'hC0400000, RNE, 0, 0, 0, 0, "SUB: 3.0 + (-3.0) = 0.0"},
        // '{32'h41200000, 32'hC0A00000, RNE, 0, 0, 0, 0, "SUB: 10.0 + (-5.0) = 5.0"},
        // '{32'h40000000, 32'hBF800000, RNE, 0, 0, 0, 0, "SUB: 2.0 + (-1.0) = 1.0"},
        // '{32'h40A00000, 32'hC0A00000, RNE, 0, 0, 0, 0, "SUB: 5.0 + (-5.0) = 0.0"},
        
        // // Negative results
        // '{32'hC0A00000, 32'h40400000, RNE, 0, 0, 0, 0, "SUB: -5.0 + 3.0 = -2.0"},
        // '{32'h40400000, 32'hC0A00000, RNE, 0, 0, 0, 0, "SUB: 3.0 + (-5.0) = -2.0"},
        
        // // Addition with zeros
        // '{32'h00000000, 32'h00000000, RNE, 0, 0, 0, 0, "ADD: +0 + +0 = +0"},
        // '{32'h80000000, 32'h80000000, RNE, 0, 0, 0, 0, "ADD: -0 + -0 = -0"},
        // '{32'h00000000, 32'h80000000, RNE, 0, 0, 0, 0, "ADD: +0 + -0 = +0"},
        // '{32'h40A00000, 32'h00000000, RNE, 0, 0, 0, 0, "ADD: 5.0 + 0.0 = 5.0"},
        // '{32'h00000000, 32'h40A00000, RNE, 0, 0, 0, 0, "ADD: 0.0 + 5.0 = 5.0"},
        
        // // Addition with infinity
        // '{32'h7F800000, 32'h40A00000, RNE, 0, 0, 0, 0, "ADD: +inf + 5.0 = +inf"},
        // '{32'hFF800000, 32'h40A00000, RNE, 0, 0, 0, 0, "ADD: -inf + 5.0 = -inf"},
        // '{32'h40A00000, 32'h7F800000, RNE, 0, 0, 0, 0, "ADD: 5.0 + +inf = +inf"},
        // '{32'h7F800000, 32'h7F800000, RNE, 0, 0, 0, 0, "ADD: +inf + +inf = +inf"},
        // '{32'hFF800000, 32'hFF800000, RNE, 0, 0, 0, 0, "ADD: -inf + -inf = -inf"},
        // '{32'h7F800000, 32'hFF800000, RNE, 0, 0, 0, 1, "ADD: +inf + -inf = NaN (invalid)"},
        // '{32'hFF800000, 32'h7F800000, RNE, 0, 0, 0, 1, "ADD: -inf + +inf = NaN (invalid)"},
        
        // // Subtraction with infinity (opposite signs)
        // '{32'h7F800000, 32'hC0A00000, RNE, 0, 0, 0, 0, "SUB: +inf + (-5.0) = +inf"},
        // '{32'h40A00000, 32'hFF800000, RNE, 0, 0, 0, 0, "SUB: 5.0 + (-inf) = -inf"},
        // '{32'h7F800000, 32'h7F800000, RNE, 0, 0, 0, 0, "ADD: +inf + +inf = +inf"},
        // '{32'hC0A00000, 32'h7F800000, RNE, 0, 0, 0, 0, "SUB: -5.0 + +inf = +inf"},
        
        // // Addition with NaN
        // '{32'h7FC00000, 32'h40A00000, RNE, 0, 0, 0, 0, "ADD: qNaN + 5.0 = qNaN"},
        // '{32'h40A00000, 32'h7FC00000, RNE, 0, 0, 0, 0, "ADD: 5.0 + qNaN = qNaN"},
        // '{32'h7FA00000, 32'h40A00000, RNE, 0, 0, 0, 1, "ADD: sNaN + 5.0 = qNaN (invalid)"},
        // '{32'h40A00000, 32'h7FA00000, RNE, 0, 0, 0, 1, "ADD: 5.0 + sNaN = qNaN (invalid)"},
        // '{32'h7FC00000, 32'h7FC00000, RNE, 0, 0, 0, 0, "ADD: qNaN + qNaN = qNaN"},
        
        // // Addition causing overflow
        // '{32'h7F7FFFFF, 32'h7F7FFFFF, RNE, 1, 0, 1, 0, "ADD: max + max = +inf (overflow)"},
        // '{32'hFF7FFFFF, 32'hFF7FFFFF, RNE, 1, 0, 1, 0, "ADD: -max + -max = -inf (overflow)"},
        
        // Small number operations (inexact flag testing)
        '{32'h3F800000, 32'h33800000, RNE, 0, 0, 1, 0, "ADD: 1.0 + 2^-24 (inexact)"},
        '{32'h3F800000, 32'hB3800000, RNE, 0, 0, 1, 0, "SUB: 1.0 + (-2^-24) (inexact)"},
        
        // Massive cancellation (normalization test)
        '{32'h3F800001, 32'hBF800000, RNE, 0, 0, 0, 0, "SUB: 1.00000012 + (-1.0) = 2^-23 (cancellation)"},
        '{32'h40000000, 32'hBFFFFFF0, RNE, 0, 0, 0, 0, "SUB: 2.0 + (-1.999998) (cancellation)"},
        
        // ===== ROUNDING MODE TESTS =====
        
        // Round to nearest even - tiny number gets absorbed
        '{32'h3F800000, 32'h34000000, RNE, 0, 0, 1, 0, "ADD RNE: 1.0 + 2^-23 (round to even)"},
        
        // Round toward zero - always truncate
        '{32'h3F800000, 32'h34000000, RTZ, 0, 0, 1, 0, "ADD RTZ: 1.0 + 2^-23 (truncate)"},
        
        // Round down (toward -inf)
        '{32'h3F800000, 32'h34000000, RDN, 0, 0, 1, 0, "ADD RDN: 1.0 + 2^-23 (positive, round down)"},
        '{32'hBF800000, 32'hB4000000, RDN, 0, 0, 1, 0, "ADD RDN: -1.0 + -2^-23 (negative, round down away from zero)"},
        
        // Round up (toward +inf)
        '{32'h3F800000, 32'h34000000, RUP, 0, 0, 1, 0, "ADD RUP: 1.0 + 2^-23 (positive, round up)"},
        '{32'hBF800000, 32'hB4000000, RUP, 0, 0, 1, 0, "ADD RUP: -1.0 + -2^-23 (negative, round toward zero)"},
        
        // ===== ALIGNMENT TESTS =====
        
        // Large exponent difference - smaller operand gets absorbed
        '{32'h42C80000, 32'h3F800000, RNE, 0, 0, 0, 0, "ADD: 100.0 + 1.0 = 101.0 (exact)"},
        '{32'h47800000, 32'h3F800000, RNE, 0, 0, 1, 0, "ADD: 65536.0 + 1.0 = 65536.0 (1.0 absorbed, inexact)"},
        '{32'h3F800000, 32'h47800000, RNE, 0, 0, 1, 0, "ADD: 1.0 + 65536.0 = 65536.0 (1.0 absorbed, inexact)"},
        
        // Negative number addition
        '{32'hC0000000, 32'hC0400000, RNE, 0, 0, 0, 0, "ADD: -2.0 + -3.0 = -5.0"},
        '{32'hBF800000, 32'hBF800000, RNE, 0, 0, 0, 0, "ADD: -1.0 + -1.0 = -2.0"}
    };

    logic[31:0] expected_result;
    initial begin
        $timeformat(-9, 0, " ns");
        rst <= 1;
        valid_data_in <= 0;
        in1 = 0;
        in2 = 0;
        rounding_mode = RNE;
        @(posedge clk);
        rst <= 0;
        @(posedge clk);
        
        foreach(test_cases[i]) begin
            @(posedge clk);
            in1 <= test_cases[i].in1;
            in2 <= test_cases[i].in2;
            expected_result <= $shortrealtobits($bitstoshortreal(test_cases[i].in1) + $bitstoshortreal(test_cases[i].in2));
            rounding_mode <= test_cases[i].rmode;
            valid_data_in <= 1;
            @(posedge clk);
            $display("[%0t] Test %0d: %s", $time, i, test_cases[i].description);
            valid_data_in <= 0;
            @(posedge valid_data_out);
            if((out != expected_result) || (overflow != test_cases[i].exp_overflow) 
            || (underflow != test_cases[i].exp_underflow) || (inexact != test_cases[i].exp_inexact)
            || (invalid_operation != test_cases[i].exp_invalid)) begin
                $display("  Result:   sign: %b exponent: %b mantissa: %b [ovf=%b unf=%b inx=%b inv=%b] [guard=%b round=%b sticky=%b normalized_mantissa= %b]", 
                    out[31], out[30:23], out[22:0], overflow, underflow, inexact, invalid_operation, guard, round, sticky, normalized_mantissa);
                $display("  Expected: sign: %b exponent: %b mantissa: %b [ovf=%b unf=%b inx=%b inv=%b]", 
                    expected_result[31], expected_result[30:23], expected_result[22:0], 
                    test_cases[i].exp_overflow, test_cases[i].exp_underflow, 
                    test_cases[i].exp_inexact, test_cases[i].exp_invalid);
            end
        end

        $display("Tests completed.");
        disable generate_clock;
    end
endmodule