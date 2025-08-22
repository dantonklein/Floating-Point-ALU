module floating_point_add (
    //input logic clk, rst,
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic[2:0] exception 
);
    logic sign_bit1, sign_bit2;
    logic[7:0] exponent1, exponent2;
    logic[22:0] mantissa1, mantissa2;
    logic[1:0] add_or_sub;//00 is both positive, 01 is sub 1 from 2, 10 is sub 2 from 1, 11 is both negative
    logic[7:0] shift_amount;
    logic number_to_shift; //0 shift number1, 1 shift number 2
    assign {sign_bit1, exponent1, mantissa1} = in1;
    assign {sign_bit2, exponent2, mantissa2} = in2;

    assign add_or_sub = {sign_bit1,sign_bit2};
    logic[22:0] smaller_number_mantissa;
    logic[23:0] larger_number_mantissa;
    logic[7:0] larger_number_exponent;
    always_comb begin
        if(exponent1 > exponent2) begin
            shift_amount = exponent1 - exponent2;
            number_to_shift = 1'b1;
            smaller_number_mantissa = mantissa2;
            larger_number_mantissa = {1'b1,mantissa1};
            larger_number_exponent = exponent1;
        end
        else begin
            shift_amount = exponent2 - exponent1;
            number_to_shift = 1'b0;
            smaller_number_mantissa = mantissa1;
            larger_number_mantissa = {1'b1,mantissa2};
            larger_number_exponent = exponent2;
        end
    end

    //Mantissa width: 23 + 1 implied bit
    //Max shift amount: 31
    //3 bits for Guard, Round, and Sticky Bit (Rounding mode decides how to handle this)
    localparam extended_width = 23 + 1 + 31 + 3;

    logic[23:0] shifted_mantissa;

    logic [extended_width-1:0] extended_mantissa, extended_shifted_mantissa;
    logic guard_bit, round_bit, sticky_bit;

    always_comb begin
        extended_mantissa = {1'b1, smaller_number_mantissa, 34'd0};
        extended_shifted_mantissa = extended_mantissa >> shift_amount;
        shifted_mantissa = extended_shifted_mantissa[57:34];

        {guard_bit, round_bit} = extended_shifted_mantissa[33:32];
        sticky_bit = | extended_shifted_mantissa[31:0]; 
    end
endmodule
