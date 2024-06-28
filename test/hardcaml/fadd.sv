module fadd (input clk, carry, input [1:0] data, output reg [1:0] sum);

   (* attribute, Kogge_stone *)
   
   always @(posedge clk) sum <= {1'b0, data[0]} + (* mode = "cla" * ) {1'b0, data[1]} + (* mode = "cla" * ) {1'b0, carry};

endmodule // fadd
