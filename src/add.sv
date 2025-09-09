module floating_point_add (
    //input logic clk, rst,
    input logic[31:0] in1, in2,
    input logic[2:0] rounding_mode,
    output logic[31:0] out,
    output logic overflow, underflow, inexact
);
    logic sign_bit1, sign_bit2;
    logic[7:0] exponent1, exponent2;
    logic[22:0] mantissa1, mantissa2;
    logic[1:0] isNegativeLargerSmaller;//00 is both positive, 01 larger is positive smaller is negative, 10 is , 11 is both negative
    logic[7:0] shift_amount;
    logic number_to_shift; //0 shift number1, 1 shift number 2
    assign {sign_bit1, exponent1, mantissa1} = in1;
    assign {sign_bit2, exponent2, mantissa2} = in2;

    
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
            isNegativeLargerSmaller = {sign_bit1, sign_bit2};
        end
        else begin
            shift_amount = exponent2 - exponent1;
            number_to_shift = 1'b0;
            smaller_number_mantissa = mantissa1;
            larger_number_mantissa = {1'b1,mantissa2};
            larger_number_exponent = exponent2;
            isNegativeLargerSmaller = {sign_bit2, sign_bit1};
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

    //Im going to have both addition and subtraction be calculated at the same time
    //This is to allow them to run in parallel paths and have a mux select which result to use
    //This may seem pointless now but it will be more important once I pipeline this

    //Add
    logic new_guard_bit_add, new_round_bit_add, new_sticky_bit_add;
    logic is_overflow_add;
    logic[23:0] mantissa_result_add;
    logic[22:0] mantissa_result_final_add; // 1 left of the decimal point isnt represented
    always_comb begin
        {is_overflow_add, mantissa_result_add} = {1'b0, larger_number_mantissa} + {1'b0, shifted_mantissa};
        if(is_overflow_add) begin
            {mantissa_result_final_add, new_guard_bit_add} = mantissa_result_add;
            new_round_bit_add = guard_bit;
            new_sticky_bit_add = round_bit | sticky_bit;
        end
        else begin
            mantissa_result_final_add = mantissa_result_add[22:0];
            new_guard_bit_add = guard_bit;
            new_round_bit_add = round_bit;
            new_sticky_bit_add = sticky_bit;
        end
    end

    //Sub
    logic new_guard_bit_sub, new_round_bit_sub, new_sticky_bit_sub;
    logic[26:0] mantissa_result_sub;
    logic[26:0] normalized_mantissa_result_sub;
    logic[22:0] mantissa_result_final_sub; // 1 left of the decimal point isnt represented
    logic[4:0] normalization_shift_amount;
    assign mantissa_result_sub = {larger_number_mantissa, 3'b000} - {shifted_mantissa, guard_bit, round_bit, sticky_bit};
    leading_zero_detection leading_zero_detector(.sub_result(mantissa_result_sub), .shift_amount(normalization_shift_amount));
    always_comb begin
        normalized_mantissa_result_sub = mantissa_result_sub << normalization_shift_amount;
        mantissa_result_final_sub = normalized_mantissa_result_sub[25:3];
        new_guard_bit_sub = normalized_mantissa_result_sub[2];
        new_round_bit_sub = normalized_mantissa_result_sub[1];
        if(mantissa_result_sub > 3) new_sticky_bit_sub = 1'b1;
        else new_sticky_bit_sub = normalized_mantissa_result_sub[0] | sticky_bit;
    end

    //Decide whether your result is addition or subtraction
    logic new_guard_bit, new_round_bit, new_sticky_bit;
    logic [22:0] mantissa_result_pre_round;
    always_comb begin
        logic add; 
        logic subtract;
        subtract = isNegativeLargerSmaller[1] ^ isNegativeLargerSmaller[0];
        if(subtract) begin
            mantissa_result_pre_round = 
        end
        else begin

        end
    end

endmodule
