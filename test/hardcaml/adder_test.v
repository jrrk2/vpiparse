module creat (
    input clear,
    input clock,
    input [7:0] b,
    input [7:0] a,
    output logic [7:0] c_wire,
    output reg [7:0] c_reg);

    always @(posedge clock)
        if (clear)
            c_reg <= 8'b00000000;
        else
            c_reg <= ( a == b ) ? { a[6:0], 1'b0 } : ( a + b );

    always_comb
        c_wire =  ( a == b ) ? { a[6:0], 1'b0 } : ( a + b );

endmodule
