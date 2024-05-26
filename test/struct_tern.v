module mult(
    output wire [3:0] out1,
    input wire [3:0] arg1, arg2);
   
   assign out1 = arg1*arg2;

endmodule

module add(
    output wire [3:0] out1,
    input wire [3:0] arg1, arg2);
   
   assign out1 = arg1+arg2;

endmodule

module top(
    output wire [3:0] out1,
    input wire [3:0] arg1, arg2, arg3);

   wire [3:0]	     tmp;

   mult mult1(.out1(tmp), .arg1, .arg2);

   add add1(.out1, .arg1(tmp), .arg2(arg3));

endmodule
	   
