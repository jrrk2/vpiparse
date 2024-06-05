module adder_test (
    input clear,
    input clock,
    input [3:0] b,
    input [7:0] a,
    output reg [7:0] d,
    output reg [8:0] d_reg);

    always @(posedge clock)
        if (clear)
            d_reg <= 8'b00000000;
        else
            d_reg <= ( a == b ) ? { a[6:0], 1'b0 } : ( a + b );

    always_comb
        d =  ( a == b ) ? { a[6:0], 1'b0 } : ( a + b );

endmodule
