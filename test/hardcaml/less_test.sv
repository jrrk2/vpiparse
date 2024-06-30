module less_test (
    input [7:0] a,
    input [7:0] b,
    output reg c);

    always_comb
          c = a < b;

endmodule
