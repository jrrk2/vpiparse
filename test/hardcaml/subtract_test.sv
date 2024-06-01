module subtract_test (
    input clk,
    input [7:0] a,
    input [7:0] b,
    output reg [7:0] c);

    always @(posedge clk)
          c <= a - b;

endmodule