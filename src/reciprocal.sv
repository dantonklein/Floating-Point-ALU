module mantissa_reciprocal_24bit_LUT (
    input logic clk, rst,
    input logic[7:0] in,
    output logic[23:0] out
);
    logic[23:0] lut[256];

    initial begin
        $readmemh("reciprocal_lut256.mem", lut);
    end

    always_ff @(posedge clk) begin
        if(rst) begin
            out <= 0;
        end else begin
            out <= lut[in];
        end
    end
endmodule

module mantissa_reciprocal_24bit (
    input logic clk, rst, valid_data_in,
    input logic[23:0] in,
    input logic[2:0] rounding_mode,
    input logic sign,
    output logic[23:0] out,
    output logic valid_data_out,
    output logic[2:0] rounding_mode_out,
    output logic sign_out 
);
//whats cool about this reciprocal is that the inputs will always be in [1,2)
//the lut output will be in (0.5,1]
//the pipeline: 1 cycle of lookup table and 2 iterations of newtons method xn+1 = xn(2-axn) which take 7 cycles each
//input is Q1.23 wide
//stage 0: look up table
//FIRST NEWTONS METHOD ITERATION
//stage 1-2: y = a * xn
//stage 3: z = 2 - y
//stage 4: round
//stage 5-6: xn+1 = xn * z
//stage 7: round
//SECOND NEWTONS METHOD ITERATION
//stage 8-9 y2 = a * xn+1
//stage 10 z2 = 2 - y2
//stage 11 round
//stage 12-13: out = xn+1 * z2
//stage 14 round

//handle valid_data_flag and have rounding_modes available on all stages
logic valid_data_ins[15];
logic[2:0] rounding_modes[15];
logic signs[15];

assign valid_data_ins[0] = valid_data_in;
assign rounding_modes[0] = rounding_mode;
assign signs[0] = sign;

genvar i;
generate
    for(i = 0; i < 15; i++) begin
        always_ff @(posedge clk or posedge rst) begin
            if(rst) begin
                valid_data_ins[i+1] <= 0;
                rounding_modes[i+1] <= 0;
                signs[i+1] <= 0;
            end else begin
                valid_data_ins[i+1] <= valid_data_ins[i];
                rounding_modes[i+1] <= rounding_modes[i];
                signs[i+1] <= signs[i];
            end
        end
end
endgenerate

assign valid_data_out = valid_data_ins[15];
assign rounding_mode_out = rounding_modes[15];

//stage 0: read from lookup table
logic[7:0] s0_input_slice;
assign s0_input_slice = in[22:15];
mantissa_reciprocal_24bit s0_reciprocal_lut(.clk(clk), .rst(rst), .in(s0_input_slice), .out(s1_x_n));

//fixed point Q1.23
logic[23:0] s1_x_n, s2_x_n, s3_x_n, s4_x_n, s5_x_n;
logic[23:0] s1_a;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s1_a <= 0;
    end else begin
        s1_a <= in;
    end
end

//stage 1: y = a * xn


//fixed point Q2.46
logic [47:0] s3_y;
Dadda_Multiplier_24bit_pipelined s1_mult(.clk(clk), .rst(rst), in1(s1_a), in2(s1_x_n), out(s3_y));


//stage 3: z = 2 - y

//fixed point Q2.26
logic [27:0] s3_y_truncated_and_negated;
logic [27:0] two;

//the reason 3 extra bits to the right of the decimal point are maintained is to avoid needing to round
assign s3_y_truncated_and_negated = ~(s3_y[47:20]) + 1'b1;
assign two = 28'h8000000;

logic [27:0] s3_z;
KSA_nbits #(.WIDTH(28)) s3_subtractor(.in1(two), .in2(s3_y_truncated_and_negated), .out(s3_z));

//fixed point Q1.26
logic [26:0] s4_z;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s2_x_n <= 0;
        s3_x_n <= 0;
        s4_x_n <= 0;
        s5_x_n <= 0;
        s4_z <= 0;
    end else begin
        s2_x_n <= s1_x_n;
        s3_x_n <= s2_x_n;
        s4_x_n <= s3_x_n;
        s5_x_n <= s4_x_n;
        s4_z <= s3_z[26:0];
    end
end
//stage 4
logic s4_guard, s4_round, s4_sticky;
//fixed point Q1.23
logic[23:0] s4_z_rounder_input, s4_z_rounded;
logic[2:0] s4_rounding_mode;
logic s4_sign;

assign {s4_z_rounder_input, s4_guard, s4_round, s4_sticky} = s4_z;
assign s4_rounding_mode = rounding_modes[4];
assign s4_sign = signs[4];

q1_23_fixed_point_rounder s4_rounder(.in(s4_z_rounder_input), .guard(s4_guard), .round(s4_round), .sticky(s4_sticky), .sign(s4_sign), .rounding_mode(s4_rounding_mode), .out(s4_z_rounded));

logic[23:0] s5_z;
always_ff @(posedge clk or posedge rst) begin
    if(rst) begin
        s5_x_n <= 0;
        s5_z <= 0;
    end else begin
        s5_x_n <= s4_x_n;
        s5_z <= s4_z_rounded;
    end
end

//stage 5

logic[47:0] s7_x_n_1
Dadda_Multiplier_24bit_pipelined s5_mult(.clk(clk), .rst(rst), in1(s5_x_n), in2(s5_z), out(s7_x_n_1));

//stage 7

logic s7_guard, s7_round, s7_sticky;
//fixed point Q1.23
logic[23:0] s7_x_n_1_rounder_input, s7_x_n_1_rounded;
logic[2:0] s7_rounding_mode;
logic s7_sign;
assign s7_rounding_mode = rounding_modes[7];
assign s7_sign = signs[7];


assign s7_x_n_1_rounder_input = s7_x_n_1[46:23];
assign s7_guard = s7_x_n_1[22];
assign s7_round = s7_x_n_1[21];
assign s7_sticky = | s7_x_n_1[20:0];


q1_23_fixed_point_rounder s7_rounder(.in(s7_x_n_1_rounder_input), .guard(s7_guard), .round(s7_round), .sticky(s7_sticky), .sign(s7_sign), .rounding_mode(s7_rounding_mode), .out(s7_x_n_1_rounded));

logic[23:0] s8_x_n_1;

endmodule