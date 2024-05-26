module add(
    output logic [3:0] out1,
    output logic [3:0] out2,
    input wire [3:0] arg1,
    input wire [3:0] arg2);

   always @(arg1 or arg2)
     begin
	out1 = arg1;
	out2 = arg2;
     end
   
endmodule
	   
