module optest0_15(clk, mode, u1, s1, u2, s2, y);

input clk;
input [6:0] mode;

input [3:0] u1, u2;
input signed [3:0] s1, s2;

output reg [7:0] y;

always @(posedge clk) begin
	y <= 8'h42;
	case (mode)
		 0: y <= u1 << u2;
		 1: y <= u1 << s2;
		 2: y <= s1 << u2;
		 3: y <= s1 << s2;

		 4: y <= u1 >> u2;
		 5: y <= u1 >> s2;
		 6: y <= s1 >> u2;
		 7: y <= s1 >> s2;

		 8: y <= u1 <<< u2;
		 9: y <= u1 <<< s2;
		10: y <= s1 <<< u2;
		11: y <= s1 <<< s2;

		12: y <= u1 >>> u2;
		13: y <= u1 >>> s2;
		14: y <= s1 >>> u2;
		15: y <= s1 >>> s2;
	endcase
end

endmodule
