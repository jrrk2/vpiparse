module creat (
    input [7:0]		a,
    input [7:0]		b,
    input [7:0]		c,
    input [7:0]		d,
    output logic [15:0]	e_wire);

    always_comb
      d_wire =  { a, b, c } + d;

endmodule
