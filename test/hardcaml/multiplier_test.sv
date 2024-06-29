module multiplier_test (
    input [3:0] b,
    input [7:0] a,
    output reg [11:0] mul);

    always_comb
          mul = a * b;

endmodule
