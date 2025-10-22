import fp_pkg::*;

module floating_point_rounder (
    input logic[22:0] mantissa,
    input logic guard, round, sticky, sign,
    input logic rounding_mode,
    output logic[23:0] rounded_mantissa_pre_overflow_detection
);
    logic[23:0] mantissa_plus_one;
    assign mantissa_plus_one = {1'b0, mantissa} + 1'b1;

    logic round_up;
    always_comb begin
        case(rounding_mode) 
            RNE: begin
                round_up = guard & (round | sticky | mantissa[0]);
            end
            RTZ: begin
                round_up = 1'b0;
            end
            RDN: begin
                round_up = sign & (guard | round | sticky);
            end
            RUP: begin
                round_up = ~sign & (guard | round | sticky);
            end
            RMM: begin
                round_up = guard;
            end
            default: begin
                round_up = 1'b0;
            end
        endcase
    end

    assign rounded_mantissa_pre_overflow_detection = round_up ? mantissa_plus_one : {1'b0,mantissa};

endmodule