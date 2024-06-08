module multiplier_signed (
    input clock,
    input signed [3:0] b,
    input signed [7:0] a,
    output reg [11:0] y);

    always @(posedge clock)
          y <= a * b;

endmodule
