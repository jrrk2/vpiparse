module multiplier_test (
    input [6:0] b,
    input [6:0] a,
    output reg [6:0] mul);

    always_comb
          mul = a * b;

endmodule
