package fp_pkg;

//floating point number
typedef struct packed {
    logic sign;
    logic [7:0] exponent;
    logic [22:0] mantissa;
} fp_32b_t;

//rounding modes
localparam logic[2:0] RNE = 3'b000;
localparam logic[2:0] RTZ = 3'b001;
localparam logic[2:0] RDN = 3'b010;
localparam logic[2:0] RUP = 3'b011;
localparam logic[2:0] RMM = 3'b100;



endpackage