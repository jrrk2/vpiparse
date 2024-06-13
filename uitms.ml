open Input_dump
open Input_types

let uitms =
{io =
  {contents =
    [("RXFINISHED",
      ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SP", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EPS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PEN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("STB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WLS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXCLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXCLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXFINISHED",
      ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BC", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SP", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EPS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PEN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("STB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WLS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXSTART", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXCLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("USAGE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FULL", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EMPTY", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("READ", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("USAGE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FULL", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EMPTY", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("READ", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BAUDTICK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIVIDER", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("INT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("IIR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("MSR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("AFE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CTI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RDA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("THI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("LSR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("IER", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DCDN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DSRN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CTSN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DTRN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RTSN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("OUT2N", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("OUT1N", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("INT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PRDATA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PWDATA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PADDR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PWRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PENABLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PSEL", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RSTN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DOWN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("ENABLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("LOAD", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WEB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SSRB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("ENB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIPB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("ADDRB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLKB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WEA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SSRA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("ENA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIPA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("ADDRA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DOA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLKA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("USAGE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("READ", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SAMPLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BAUDTICK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIVIDER", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("INT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("IIR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("MSR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("AFE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CTI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RDA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("THI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("LSR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("IER", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SAMPLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("OVERFLOW", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DOWN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("ENABLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("LOAD", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXFINISHED",
      ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SP", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EPS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PEN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("STB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WLS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXCLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXCLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXFINISHED",
      ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BC", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SP", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EPS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PEN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("STB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WLS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXSTART", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXCLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SAMPLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("OVERFLOW", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DOWN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("ENABLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("LOAD", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXFINISHED",
      ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SP", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EPS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PEN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("STB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WLS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXCLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RXCLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXFINISHED",
      ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BC", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SP", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EPS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PEN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("STB", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WLS", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXSTART", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("TXCLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("USAGE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FULL", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EMPTY", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("READ", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("USAGE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FULL", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("EMPTY", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("READ", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("WRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("BAUDTICK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DIVIDER", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLEAR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("FE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("INT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("IIR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("MSR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("AFE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CTI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RDA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("THI", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("LSR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("IER", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("Q", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("D", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RST", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SOUT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("SIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DCDN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DSRN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CTSN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("DTRN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RTSN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("OUT2N", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("OUT1N", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("INT", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PRDATA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PWDATA", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PADDR", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PWRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PENABLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("PSEL", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("RSTN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
     ("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []))]};
 v =
  {contents =
    [("tx_State",
      ("", (BASDTYP, "logic", TYPNONE, []), "logic",
       (UNKDTYP, "", TYPNONE, [])));
     ("rx_State",
      ("", (BASDTYP, "logic", TYPNONE, []), "logic",
       (UNKDTYP, "", TYPNONE, [])));
     ("NState",
      ("", (BASDTYP, "logic", TYPNONE, []), "logic",
       (UNKDTYP, "", TYPNONE, [])));
     ("CState",
      ("", (BASDTYP, "logic", TYPNONE, []), "logic",
       (UNKDTYP, "", TYPNONE, [])));
     ("NState",
      ("", (BASDTYP, "logic", TYPNONE, []), "logic",
       (UNKDTYP, "", TYPNONE, [])));
     ("CState",
      ("", (BASDTYP, "logic", TYPNONE, []), "logic",
       (UNKDTYP, "", TYPNONE, [])))]};
 iv = {contents = []}; ir = {contents = []}; ca = {contents = []};
 alwys =
  {contents =
    [("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iMSR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iLSR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iIIR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iRBR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       CS ("",
        [VRF ("PADDR", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (3, STRING "000");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iRBR", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iDLL", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])])])]);
         CSITM ("",
          [CNST (3, STRING "001");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iIER", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iDLM", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])])])]);
         CSITM ("",
          [CNST (3, STRING "010");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "011");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iLCR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "100");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iMCR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "101");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iLSR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "110");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iMSR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "111");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iSCR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [UNKNOWN;
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iRBR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBaudtick16x", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iMCR_OUT1", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iMCR_OUT2", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iMCR_DTR", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iSOUT", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")]);
                 LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Ceq,
                    [VRF ("iRXFIFOTrigger", (BASDTYP, "logic", TYPNONE, []),
                      []);
                     CNST (1, STRING "1")])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "0");
                   VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     LOGIC (Lor,
                      [CMP (Ceq,
                        [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (1, STRING "0")]);
                       LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (1, STRING "1")]);
                         CMP (Ceq,
                          [VRF ("iRXFIFOEmpty",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "1")])])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iRXFIFOD", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
             CS ("",
              [VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), []);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CAT ("",
                          [VRF ("iRXBI", (BASDTYP, "logic", TYPNONE, []), []);
                           CAT ("",
                            [VRF ("iRXFE", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CAT ("",
                              [VRF ("iRXPE", (BASDTYP, "logic", TYPNONE, []),
                                []);
                               VRF ("iRXData",
                                (BASDTYP, "logic", TYPNONE, []), [])])])]);
                         VRF ("iRXFIFOD", (BASDTYP, "logic", TYPNONE, []),
                          [])]);
                       IF ("",
                        [CMP (Ceq,
                          [VRF ("iFCR_FIFOEnable",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         BGN (None,
                          [ASGN (false, "",
                            [CNST (1, STRING "1");
                             VRF ("iRXFIFOClear",
                              (BASDTYP, "logic", TYPNONE, []), [])])]);
                         BGN (None, [])]);
                       ASGN (false, "",
                        [VRF ("RXSAVE", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iFCR_FIFOEnable",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     BGN (None,
                      [ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("iRXFIFOWrite",
                          (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None,
                      [IF ("",
                        [CMP (Ceq,
                          [VRF ("iRXFIFOFull",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         BGN (None,
                          [ASGN (false, "",
                            [CNST (1, STRING "1");
                             VRF ("iRXFIFOWrite",
                              (BASDTYP, "logic", TYPNONE, []), [])])]);
                         BGN (None, [])])])]);
                   ASGN (false, "",
                    [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               CSITM ("",
                [UNKNOWN;
                 ASGN (false, "",
                  [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iTSR", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
             CS ("",
              [VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), []);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iTXEnable", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []),
                          [])]);
                       ASGN (false, "",
                        [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [VRF ("iTXFIFOQ", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("iTSR", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [VRF ("TXRUN", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iTXFinished", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("TXEND", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("TXRUN", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 ASGN (false, "",
                  [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])])]);
               CSITM ("",
                [UNKNOWN;
                 ASGN (false, "",
                  [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iSCR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iSCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iSCR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iCTSnRE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iCTSnFE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iDSRnRE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iDSRnFE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iRInFE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iDCDnRE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iDCDnFE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iFCR_FIFOEnable",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])]);
                 LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iFCR_FIFOEnable",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iPERE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iFERE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBIRE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Cneq,
                [VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 0)]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lor,
                    [CMP (Ceq,
                      [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [IRNG ("", [CNST (32, HEX 10); CNST (32, HEX 8)]);
                       CNST (3, STRING "000")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "0")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (32, HEX 1)]);
                       VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None,
                    [IF ("",
                      [LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iFEIncrement",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         CMP (Ceq,
                          [VRF ("iFEDecrement",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "1")])]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [CNST (32, HEX 1);
                           VRF ("iFECounter",
                            (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None, [])])])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               IRNG ("", [CNST (32, HEX 5); CNST (32, HEX 0)])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iMCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 5); CNST (32, HEX 0)]);
                   IRNG ("", [CNST (32, HEX 5); CNST (32, HEX 0)])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iLCR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iLCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iLCR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iFCR_RXTrigger", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iFCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [SEL ("",
                    [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (32, HEX 0)]);
                   VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                    [])]);
                 ASGN (false, "",
                  [SEL ("",
                    [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (32, HEX 3)]);
                   VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 6)]);
                   VRF ("iFCR_RXTrigger", (BASDTYP, "logic", TYPNONE, []),
                    [])]);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [SEL ("",
                        [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 5)]);
                       VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])]);
                 IF ("",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 1)]);
                         CNST (1, STRING "1")]);
                       LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iFCR_FIFOEnable",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         CMP (Ceq,
                          [SEL ("",
                            [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CNST (32, HEX 0)]);
                           CNST (1, STRING "1")])])]);
                     LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iFCR_FIFOEnable",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 0)]);
                         CNST (1, STRING "0")])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iFCR_RXFIFOReset",
                        (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])]);
                 IF ("",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 2)]);
                         CNST (1, STRING "1")]);
                       LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iFCR_FIFOEnable",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         CMP (Ceq,
                          [SEL ("",
                            [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CNST (32, HEX 0)]);
                           CNST (1, STRING "1")])])]);
                     LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iFCR_FIFOEnable",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 0)]);
                         CNST (1, STRING "0")])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iFCR_TXFIFOReset",
                        (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 5)]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iTimeoutCount", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [CMP (Ceq,
                    [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                      []);
                     CNST (1, STRING "1")]);
                   CMP (Ceq,
                    [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])]);
                 CMP (Ceq,
                  [VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 5)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                   VRF ("iTimeoutCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lredand,
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iRXFIFOEmpty",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Ceq,
                        [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (1, STRING "1")])]);
                     CMP (Ceq,
                      [SEL ("",
                        [VRF ("iTimeoutCount",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 5)]);
                       CNST (1, STRING "0")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iTimeoutCount",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 1)]);
                       VRF ("iTimeoutCount", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None,
                    [IF ("",
                      [CMP (Ceq,
                        [SEL ("",
                          [VRF ("iTimeoutCount",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (32, HEX 5)]);
                         CNST (1, STRING "1")]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [CNST (1, STRING "1");
                           VRF ("iCharTimeout",
                            (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None, [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "0");
                   VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [CMP (Ceq,
                    [VRF ("iLSR_THRERE", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Ceq,
                    [VRF ("iFCR_TXFIFOReset",
                      (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])]);
                 LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [SEL ("",
                        [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 1)]);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iLSR_THRE", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lor,
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iIIRRead", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (1, STRING "1")]);
                       CMP (Ceq,
                        [IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 1)]);
                         CNST (3, STRING "001")])]);
                     CMP (Ceq,
                      [VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 0)])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 0)]);
                   IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 0)])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iDLL", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iDLM", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDLLWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iDLL", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDLMWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iDLM", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [CMP (Ceq,
                  [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (32, HEX 0);
                     VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
             BGN (None, [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("LOAD", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("$unsigned", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [IF ("",
                      [CMP (Ceq,
                        [VRF ("ENABLE", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       BGN (None,
                        [BGN (None, []);
                         IF ("",
                          [CMP (Ceq,
                            [VRF ("DOWN", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CNST (1, STRING "0")]);
                           BGN (None,
                            [BGN (None, []);
                             ASGN (false, "",
                              [ARITH (Aadd,
                                [VRF ("iCounter",
                                  (BASDTYP, "logic", TYPNONE, []), []);
                                 CNST (32, HEX 1)]);
                               VRF ("iCounter",
                                (BASDTYP, "logic", TYPNONE, []), [])])]);
                           BGN (None,
                            [BGN (None, []);
                             ASGN (false, "",
                              [CNST (32, HEX 1);
                               VRF ("iCounter",
                                (BASDTYP, "logic", TYPNONE, []), [])])])])]);
                       BGN (None, [])])])])])]);
             IF ("",
              [CMP (Ceq,
                [SEL ("",
                  [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("WIDTH", (BASDTYP, "logic", TYPNONE, []), [])]);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   SEL ("",
                    [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("WIDTH", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, [VRF ("size64", (BASDTYP, "logic", TYPNONE, []), [])]);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, [VRF ("size64", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)]);
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, [VRF ("size64", (BASDTYP, "logic", TYPNONE, []), [])]);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               CMP (Ceq,
                [VRF ("iFULL", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")])]);
             BGN (None,
              [BGN (None,
                [VRF ("size64", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                 SEL ("",
                  [VRF ("iFIFOMem", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 1); CNST (32, HEX 0)])])])]);
             BGN (None, [])]);
           ASGN (false, "",
            [SEL ("",
              [VRF ("iFIFOMem", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 1); CNST (32, HEX 0)])]);
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iFULL", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])]);
               IF ("",
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (32, HEX 1);
                     VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "1");
             VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               CMP (Ceq,
                [VRF ("iFULL", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [ARITH (Aadd,
                  [VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               CMP (Ceq,
                [VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [ARITH (Aadd,
                  [VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Cneq,
                    [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [IF ("",
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Cneq,
                        [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 0)])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (32, HEX 1);
                         VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 0)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "0");
                     VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           ASGN (false, "",
            [SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)]);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 1)])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Cgte,
                [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("THRESHOLD", (BASDTYP, "logic", TYPNONE, []), [])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("SAMPLE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (32, HEX 1)]);
                       VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
                 ASGN (false, "",
                  [CNST (1, STRING "0");
                   VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lor,
                                  [LOGIC (Lor,
                                    [LOGIC (Lor,
                                      [LOGIC (Lor,
                                        [LOGIC (Lor,
                                          [LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 15)]);
                                           LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 14)])]);
                                         LOGIC (Lshiftl,
                                          [CNST (32, HEX 0);
                                           CNST (32, HEX 13)])]);
                                       LOGIC (Lshiftl,
                                        [CNST (32, HEX 0); CNST (32, HEX 12)])]);
                                     LOGIC (Lshiftl,
                                      [CNST (32, HEX 0); CNST (32, HEX 11)])]);
                                   LOGIC (Lshiftl,
                                    [CNST (32, HEX 0); CNST (32, HEX 10)])]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 9)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 8)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("BAUDTICK", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lor,
                                  [LOGIC (Lor,
                                    [LOGIC (Lor,
                                      [LOGIC (Lor,
                                        [LOGIC (Lor,
                                          [LOGIC (Lor,
                                            [LOGIC (Lor,
                                              [LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 15)]);
                                               LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 14)])]);
                                             LOGIC (Lshiftl,
                                              [CNST (32, HEX 0);
                                               CNST (32, HEX 13)])]);
                                           LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 12)])]);
                                         LOGIC (Lshiftl,
                                          [CNST (32, HEX 0);
                                           CNST (32, HEX 11)])]);
                                       LOGIC (Lshiftl,
                                        [CNST (32, HEX 0); CNST (32, HEX 10)])]);
                                     LOGIC (Lshiftl,
                                      [CNST (32, HEX 0); CNST (32, HEX 9)])]);
                                   LOGIC (Lshiftl,
                                    [CNST (32, HEX 0); CNST (32, HEX 8)])]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 7)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                 VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("BAUDTICK", (BASDTYP, "logic", TYPNONE, []), [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("DIVIDER", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lor,
                                  [LOGIC (Lor,
                                    [LOGIC (Lor,
                                      [LOGIC (Lor,
                                        [LOGIC (Lor,
                                          [LOGIC (Lor,
                                            [LOGIC (Lor,
                                              [LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 15)]);
                                               LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 14)])]);
                                             LOGIC (Lshiftl,
                                              [CNST (32, HEX 0);
                                               CNST (32, HEX 13)])]);
                                           LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 12)])]);
                                         LOGIC (Lshiftl,
                                          [CNST (32, HEX 0);
                                           CNST (32, HEX 11)])]);
                                       LOGIC (Lshiftl,
                                        [CNST (32, HEX 0); CNST (32, HEX 10)])]);
                                     LOGIC (Lshiftl,
                                      [CNST (32, HEX 0); CNST (32, HEX 9)])]);
                                   LOGIC (Lshiftl,
                                    [CNST (32, HEX 0); CNST (32, HEX 8)])]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 7)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                 VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("BAUDTICK", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (4, STRING "0001");
               VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iRLSInterrupt", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (4, STRING "0110");
                   VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iCTIInterrupt", (BASDTYP, "logic", TYPNONE, []),
                      []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (4, STRING "1100");
                       VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [IF ("",
                      [CMP (Ceq,
                        [VRF ("iRDAInterrupt",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [CNST (4, STRING "0100");
                           VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None,
                        [IF ("",
                          [CMP (Ceq,
                            [VRF ("iTHRInterrupt",
                              (BASDTYP, "logic", TYPNONE, []), []);
                             CNST (1, STRING "1")]);
                           BGN (None,
                            [BGN (None, []);
                             ASGN (false, "",
                              [CNST (4, STRING "0010");
                               VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []),
                                [])])]);
                           BGN (None,
                            [IF ("",
                              [CMP (Ceq,
                                [VRF ("iMSRInterrupt",
                                  (BASDTYP, "logic", TYPNONE, []), []);
                                 CNST (1, STRING "1")]);
                               BGN (None,
                                [BGN (None, []);
                                 ASGN (false, "",
                                  [CNST (4, STRING "0000");
                                   VRF ("iIIR",
                                    (BASDTYP, "logic", TYPNONE, []), 
                                    [])])]);
                               BGN (None,
                                [BGN (None, []);
                                 ASGN (false, "",
                                  [CNST (4, STRING "0001");
                                   VRF ("iIIR",
                                    (BASDTYP, "logic", TYPNONE, []), 
                                    [])])])])])])])])])])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iParityReceived", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), [])]);
               CMP (Ceq,
                [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [VRF ("iFSIN", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("iParityReceived", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])]);
               IF ("",
                [CMP (Ceq,
                  [VRF ("SP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [LOGIC (Lxor,
                        [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("iParityReceived",
                          (BASDTYP, "logic", TYPNONE, []), [])]);
                       CNST (1, STRING "0")]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])]);
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Cneq,
                      [VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("iParityReceived",
                        (BASDTYP, "logic", TYPNONE, []), [])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("iParityReceived", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("STB", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("WLS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("PEN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iDataCountFinish",
            (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iBaudCount", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("iBaudStep", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("iFStopBit", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("iFSIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("SIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("CState", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       ASGN (false, "",
        [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "0");
         VRF ("iBaudCountClear", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "0");
         VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "0");
         VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
       CS ("",
        [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("SIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iBaudCountClear", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iFSIN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("DATA", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDataCountFinish", (BASDTYP, "logic", TYPNONE, []),
                  []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("DATA", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [SEL ("",
                  [VRF ("iBaudCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 3)]);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iFStopBit", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []),
                        [])]);
                     ASGN (false, "",
                      [VRF ("MWAIT", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []),
                        [])]);
                     ASGN (false, "",
                      [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("SIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("MWAIT", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])]);
         CSITM ("",
          [UNKNOWN;
           BGN (None, [BGN (None, []); BGN (None, [BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("NState", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []), [])]);
                 ASGN (false, "",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 7)]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                   VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("iDataCountFinish",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("iFSIN", (BASDTYP, "logic", TYPNONE, []), []);
                       SEL ("",
                        [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (32, HEX 1)]);
                       VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("iDOUT", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("EPS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       ASGN (false, "",
        [LOGIC (Lxor,
          [LOGIC (Lxor,
            [LOGIC (Lxor,
              [LOGIC (Lxor,
                [LOGIC (Lxor,
                  [LOGIC (Lxor,
                    [LOGIC (Lxor,
                      [LOGIC (Lxor,
                        [SEL ("",
                          [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (32, HEX 7)]);
                         SEL ("",
                          [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (32, HEX 6)])]);
                       SEL ("",
                        [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 5)])]);
                     SEL ("",
                      [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 4)])]);
                   SEL ("",
                    [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (32, HEX 3)])]);
                 SEL ("",
                  [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 2)])]);
               SEL ("",
                [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 1)])]);
             SEL ("",
              [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           UNRY (Unegate, [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), [])])]);
         VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iBaudStepD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iBaudStepD", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
       BGN (None,
        [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
       IF ("",
        [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
         BGN (None,
          [BGN (None,
            [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None,
            [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               CMP (Ceq,
                [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("iFinished", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("DIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("WLS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       VRF ("TX_PAR", (BASDTYP, "logic", TYPNONE, []), []);
       BGN (None,
        [VRF ("TX_PAR", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
       VRF ("iP40", (BASDTYP, "logic", TYPNONE, []), []);
       VRF ("iP50", (BASDTYP, "logic", TYPNONE, []), []);
       VRF ("iP60", (BASDTYP, "logic", TYPNONE, []), []);
       VRF ("iP70", (BASDTYP, "logic", TYPNONE, []), []);
       LOGIC (Lxor,
        [LOGIC (Lxor,
          [LOGIC (Lxor,
            [LOGIC (Lxor,
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 4)]);
               SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 3)])]);
             SEL ("",
              [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 2)])]);
           SEL ("",
            [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
             CNST (32, HEX 1)])]);
         SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 0)])]);
       LOGIC (Lxor,
        [SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 5)]);
         VRF ("iP40", (BASDTYP, "logic", TYPNONE, []), [])]);
       LOGIC (Lxor,
        [SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 6)]);
         VRF ("iP50", (BASDTYP, "logic", TYPNONE, []), [])]);
       LOGIC (Lxor,
        [SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 7)]);
         VRF ("iP60", (BASDTYP, "logic", TYPNONE, []), [])]);
       CS ("",
        [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (2, STRING "00");
           ASGN (false, "",
            [VRF ("iP40", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
         CSITM ("",
          [CNST (2, STRING "01");
           ASGN (false, "",
            [VRF ("iP50", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
         CSITM ("",
          [CNST (2, STRING "10");
           ASGN (false, "",
            [VRF ("iP60", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
         CSITM ("",
          [UNKNOWN;
           ASGN (false, "",
            [VRF ("iP70", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("iParity", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("STB", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("EPS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("SP", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("PEN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("WLS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("DIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("TXSTART", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("CState", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       ASGN (false, "",
        [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "1");
         VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
       CS ("",
        [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT0", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 0)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT1", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 1)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT2", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 2)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT3", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 3)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT4", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 4)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (2, STRING "00")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("BIT5", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 5)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (2, STRING "01")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("BIT6", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 6)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (2, STRING "10")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("BIT7", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 7)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("SP", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [UNRY (Unegate,
                        [VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])]);
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
             ASGN (false, "",
              [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("STB", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP2", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])]);
         CSITM ("",
          [UNKNOWN;
           BGN (None, [BGN (None, []); BGN (None, [BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("TXCLK", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("NState", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])]);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     IF ("",
                      [LOGIC (Lredand,
                        [LOGIC (Lredand,
                          [CMP (Ceq,
                            [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                             CNST (2, STRING "00")]);
                           CMP (Ceq,
                            [VRF ("STB", (BASDTYP, "logic", TYPNONE, []), []);
                             CNST (1, STRING "1")])]);
                         CMP (Ceq,
                          [VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           VRF ("STOP2", (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [VRF ("NState", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            [])]);
                         ASGN (false, "",
                          [CNST (1, STRING "1");
                           VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            [])]);
                         ASGN (false, "",
                          [CNST (1, STRING "0");
                           VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Cneq,
                    [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [IF ("",
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Cneq,
                        [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 0)])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (32, HEX 1);
                         VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 0)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "0");
                     VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Cgte,
                [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("THRESHOLD", (BASDTYP, "logic", TYPNONE, []), [])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("SAMPLE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (32, HEX 1)]);
                       VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
                 ASGN (false, "",
                  [CNST (1, STRING "0");
                   VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("LOAD", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("$unsigned", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [IF ("",
                      [CMP (Ceq,
                        [VRF ("ENABLE", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       BGN (None,
                        [BGN (None, []);
                         IF ("",
                          [CMP (Ceq,
                            [VRF ("DOWN", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CNST (1, STRING "0")]);
                           BGN (None,
                            [BGN (None, []);
                             ASGN (false, "",
                              [ARITH (Aadd,
                                [VRF ("iCounter",
                                  (BASDTYP, "logic", TYPNONE, []), []);
                                 CNST (32, HEX 1)]);
                               VRF ("iCounter",
                                (BASDTYP, "logic", TYPNONE, []), [])])]);
                           BGN (None,
                            [BGN (None, []);
                             ASGN (false, "",
                              [CNST (32, HEX 1);
                               VRF ("iCounter",
                                (BASDTYP, "logic", TYPNONE, []), [])])])])]);
                       BGN (None, [])])])])])]);
             IF ("",
              [CMP (Ceq,
                [SEL ("",
                  [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("WIDTH", (BASDTYP, "logic", TYPNONE, []), [])]);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   SEL ("",
                    [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("WIDTH", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iParityReceived", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), [])]);
               CMP (Ceq,
                [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [VRF ("iFSIN", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("iParityReceived", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])]);
               IF ("",
                [CMP (Ceq,
                  [VRF ("SP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [LOGIC (Lxor,
                        [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("iParityReceived",
                          (BASDTYP, "logic", TYPNONE, []), [])]);
                       CNST (1, STRING "0")]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])]);
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Cneq,
                      [VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("iParityReceived",
                        (BASDTYP, "logic", TYPNONE, []), [])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("PE", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("iParityReceived", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("STB", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("WLS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("PEN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iDataCountFinish",
            (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iBaudCount", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("iBaudStep", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("iFStopBit", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("iFSIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("SIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("CState", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       ASGN (false, "",
        [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "0");
         VRF ("iBaudCountClear", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "0");
         VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "0");
         VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
       CS ("",
        [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("SIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iBaudCountClear", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iFSIN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("DATA", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDataCountFinish", (BASDTYP, "logic", TYPNONE, []),
                  []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("DATA", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [SEL ("",
                  [VRF ("iBaudCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 3)]);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iFStopBit", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []),
                        [])]);
                     ASGN (false, "",
                      [VRF ("MWAIT", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []),
                        [])]);
                     ASGN (false, "",
                      [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (3, STRING "5");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("SIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("MWAIT", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])]);
         CSITM ("",
          [UNKNOWN;
           BGN (None, [BGN (None, []); BGN (None, [BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("NState", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDataCountInit", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []), [])]);
                 ASGN (false, "",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 7)]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                   VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("iDataCountFinish",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("iFSIN", (BASDTYP, "logic", TYPNONE, []), []);
                       SEL ("",
                        [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (32, HEX 1)]);
                       VRF ("iDataCount", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("iDOUT", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("EPS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       ASGN (false, "",
        [LOGIC (Lxor,
          [LOGIC (Lxor,
            [LOGIC (Lxor,
              [LOGIC (Lxor,
                [LOGIC (Lxor,
                  [LOGIC (Lxor,
                    [LOGIC (Lxor,
                      [LOGIC (Lxor,
                        [SEL ("",
                          [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (32, HEX 7)]);
                         SEL ("",
                          [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (32, HEX 6)])]);
                       SEL ("",
                        [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 5)])]);
                     SEL ("",
                      [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 4)])]);
                   SEL ("",
                    [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (32, HEX 3)])]);
                 SEL ("",
                  [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 2)])]);
               SEL ("",
                [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 1)])]);
             SEL ("",
              [VRF ("iDOUT", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           UNRY (Unegate, [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), [])])]);
         VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iBaudStepD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("iBaudStep", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iBaudStepD", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
       BGN (None,
        [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
       IF ("",
        [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
         BGN (None,
          [BGN (None,
            [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None,
            [VRF ("TX_FIN", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               CMP (Ceq,
                [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("iFinished", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("iLast", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("DIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("WLS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       VRF ("TX_PAR", (BASDTYP, "logic", TYPNONE, []), []);
       BGN (None,
        [VRF ("TX_PAR", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("uart_transmitter", (BASDTYP, "logic", TYPNONE, []), [])]);
       VRF ("iP40", (BASDTYP, "logic", TYPNONE, []), []);
       VRF ("iP50", (BASDTYP, "logic", TYPNONE, []), []);
       VRF ("iP60", (BASDTYP, "logic", TYPNONE, []), []);
       VRF ("iP70", (BASDTYP, "logic", TYPNONE, []), []);
       LOGIC (Lxor,
        [LOGIC (Lxor,
          [LOGIC (Lxor,
            [LOGIC (Lxor,
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 4)]);
               SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 3)])]);
             SEL ("",
              [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 2)])]);
           SEL ("",
            [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
             CNST (32, HEX 1)])]);
         SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 0)])]);
       LOGIC (Lxor,
        [SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 5)]);
         VRF ("iP40", (BASDTYP, "logic", TYPNONE, []), [])]);
       LOGIC (Lxor,
        [SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 6)]);
         VRF ("iP50", (BASDTYP, "logic", TYPNONE, []), [])]);
       LOGIC (Lxor,
        [SEL ("",
          [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (32, HEX 7)]);
         VRF ("iP60", (BASDTYP, "logic", TYPNONE, []), [])]);
       CS ("",
        [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (2, STRING "00");
           ASGN (false, "",
            [VRF ("iP40", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
         CSITM ("",
          [CNST (2, STRING "01");
           ASGN (false, "",
            [VRF ("iP50", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
         CSITM ("",
          [CNST (2, STRING "10");
           ASGN (false, "",
            [VRF ("iP60", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])]);
         CSITM ("",
          [UNKNOWN;
           ASGN (false, "",
            [VRF ("iP70", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("iParity", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("STB", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("EPS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("SP", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("PEN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("WLS", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("DIN", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("TXSTART", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("CState", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       ASGN (false, "",
        [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])]);
       ASGN (false, "",
        [CNST (1, STRING "1");
         VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
       CS ("",
        [VRF ("CState", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT0", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 0)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT1", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 1)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT2", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 2)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT3", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 3)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("BIT4", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 4)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (2, STRING "00")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("BIT5", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 5)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (2, STRING "01")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("BIT6", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 6)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (2, STRING "10")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("BIT7", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [SEL ("",
                [VRF ("DIN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 7)]);
               VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("PEN", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("PAR", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("SP", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("EPS", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [UNRY (Unegate,
                        [VRF ("iParity", (BASDTYP, "logic", TYPNONE, []), [])]);
                       VRF ("iSout", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
             ASGN (false, "",
              [VRF ("STOP", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("STB", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("STOP2", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])])])]);
         CSITM ("",
          [CNST (4, STRING "12");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("START", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("NState", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])]);
         CSITM ("",
          [UNKNOWN;
           BGN (None, [BGN (None, []); BGN (None, [BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("IDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("TXCLK", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [VRF ("NState", (BASDTYP, "logic", TYPNONE, []), []);
                       VRF ("CState", (BASDTYP, "logic", TYPNONE, []), [])]);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [BGN (None, []);
                     IF ("",
                      [LOGIC (Lredand,
                        [LOGIC (Lredand,
                          [CMP (Ceq,
                            [VRF ("WLS", (BASDTYP, "logic", TYPNONE, []), []);
                             CNST (2, STRING "00")]);
                           CMP (Ceq,
                            [VRF ("STB", (BASDTYP, "logic", TYPNONE, []), []);
                             CNST (1, STRING "1")])]);
                         CMP (Ceq,
                          [VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           VRF ("STOP2", (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [VRF ("NState", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            [])]);
                         ASGN (false, "",
                          [CNST (1, STRING "1");
                           VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           VRF ("CState", (BASDTYP, "logic", TYPNONE, []),
                            [])]);
                         ASGN (false, "",
                          [CNST (1, STRING "0");
                           VRF ("iTx2", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iFULL", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])]);
               IF ("",
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (32, HEX 1);
                     VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "1");
             VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               CMP (Ceq,
                [VRF ("iFULL", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [ARITH (Aadd,
                  [VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               CMP (Ceq,
                [VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [ARITH (Aadd,
                  [VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iFULL", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])]);
               IF ("",
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "0")])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (32, HEX 1);
                     VRF ("iUSAGE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "1");
             VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("WRITE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               CMP (Ceq,
                [VRF ("iFULL", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [ARITH (Aadd,
                  [VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [LOGIC (Lredand,
              [CMP (Ceq,
                [VRF ("READ", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               CMP (Ceq,
                [VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [ARITH (Aadd,
                  [VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [CNST (32, HEX 0);
                 VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iRDAddr", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iWRAddr", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "0");
                 VRF ("iEMPTY", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [CMP (Ceq,
                  [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 1)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iQ", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (32, HEX 0);
                     VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
             BGN (None, [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lor,
                                  [LOGIC (Lor,
                                    [LOGIC (Lor,
                                      [LOGIC (Lor,
                                        [LOGIC (Lor,
                                          [LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 15)]);
                                           LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 14)])]);
                                         LOGIC (Lshiftl,
                                          [CNST (32, HEX 0);
                                           CNST (32, HEX 13)])]);
                                       LOGIC (Lshiftl,
                                        [CNST (32, HEX 0); CNST (32, HEX 12)])]);
                                     LOGIC (Lshiftl,
                                      [CNST (32, HEX 0); CNST (32, HEX 11)])]);
                                   LOGIC (Lshiftl,
                                    [CNST (32, HEX 0); CNST (32, HEX 10)])]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 9)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 8)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("BAUDTICK", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CLEAR", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lor,
                                  [LOGIC (Lor,
                                    [LOGIC (Lor,
                                      [LOGIC (Lor,
                                        [LOGIC (Lor,
                                          [LOGIC (Lor,
                                            [LOGIC (Lor,
                                              [LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 15)]);
                                               LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 14)])]);
                                             LOGIC (Lshiftl,
                                              [CNST (32, HEX 0);
                                               CNST (32, HEX 13)])]);
                                           LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 12)])]);
                                         LOGIC (Lshiftl,
                                          [CNST (32, HEX 0);
                                           CNST (32, HEX 11)])]);
                                       LOGIC (Lshiftl,
                                        [CNST (32, HEX 0); CNST (32, HEX 10)])]);
                                     LOGIC (Lshiftl,
                                      [CNST (32, HEX 0); CNST (32, HEX 9)])]);
                                   LOGIC (Lshiftl,
                                    [CNST (32, HEX 0); CNST (32, HEX 8)])]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 7)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                 VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("BAUDTICK", (BASDTYP, "logic", TYPNONE, []), [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("DIVIDER", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lor,
                              [LOGIC (Lor,
                                [LOGIC (Lor,
                                  [LOGIC (Lor,
                                    [LOGIC (Lor,
                                      [LOGIC (Lor,
                                        [LOGIC (Lor,
                                          [LOGIC (Lor,
                                            [LOGIC (Lor,
                                              [LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 15)]);
                                               LOGIC (Lshiftl,
                                                [CNST (32, HEX 0);
                                                 CNST (32, HEX 14)])]);
                                             LOGIC (Lshiftl,
                                              [CNST (32, HEX 0);
                                               CNST (32, HEX 13)])]);
                                           LOGIC (Lshiftl,
                                            [CNST (32, HEX 0);
                                             CNST (32, HEX 12)])]);
                                         LOGIC (Lshiftl,
                                          [CNST (32, HEX 0);
                                           CNST (32, HEX 11)])]);
                                       LOGIC (Lshiftl,
                                        [CNST (32, HEX 0); CNST (32, HEX 10)])]);
                                     LOGIC (Lshiftl,
                                      [CNST (32, HEX 0); CNST (32, HEX 9)])]);
                                   LOGIC (Lshiftl,
                                    [CNST (32, HEX 0); CNST (32, HEX 8)])]);
                                 LOGIC (Lshiftl,
                                  [CNST (32, HEX 0); CNST (32, HEX 7)])]);
                               LOGIC (Lshiftl,
                                [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                 VRF ("iCounter", (BASDTYP, "logic", TYPNONE, []), [])]);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("BAUDTICK", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None, [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iDd", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (4, STRING "0001");
               VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iRLSInterrupt", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (4, STRING "0110");
                   VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iCTIInterrupt", (BASDTYP, "logic", TYPNONE, []),
                      []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (4, STRING "1100");
                       VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None,
                    [IF ("",
                      [CMP (Ceq,
                        [VRF ("iRDAInterrupt",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [CNST (4, STRING "0100");
                           VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None,
                        [IF ("",
                          [CMP (Ceq,
                            [VRF ("iTHRInterrupt",
                              (BASDTYP, "logic", TYPNONE, []), []);
                             CNST (1, STRING "1")]);
                           BGN (None,
                            [BGN (None, []);
                             ASGN (false, "",
                              [CNST (4, STRING "0010");
                               VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []),
                                [])])]);
                           BGN (None,
                            [IF ("",
                              [CMP (Ceq,
                                [VRF ("iMSRInterrupt",
                                  (BASDTYP, "logic", TYPNONE, []), []);
                                 CNST (1, STRING "1")]);
                               BGN (None,
                                [BGN (None, []);
                                 ASGN (false, "",
                                  [CNST (4, STRING "0000");
                                   VRF ("iIIR",
                                    (BASDTYP, "logic", TYPNONE, []), 
                                    [])])]);
                               BGN (None,
                                [BGN (None, []);
                                 ASGN (false, "",
                                  [CNST (4, STRING "0001");
                                   VRF ("iIIR",
                                    (BASDTYP, "logic", TYPNONE, []), 
                                    [])])])])])])])])])])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Cneq,
                    [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [IF ("",
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Cneq,
                        [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 0)])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (32, HEX 1);
                         VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 0)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "0");
                     VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Cneq,
                    [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [IF ("",
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Cneq,
                        [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 0)])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (32, HEX 1);
                         VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 0)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "0");
                     VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Cneq,
                    [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [IF ("",
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Cneq,
                        [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 0)])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (32, HEX 1);
                         VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 0)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "0");
                     VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [CNST (32, HEX 0);
             VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])]);
           ASGN (false, "",
            [CNST (1, STRING "0");
             VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           IF ("",
            [CMP (Ceq,
              [VRF ("CE", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (1, STRING "1")]);
             BGN (None,
              [BGN (None, []);
               IF ("",
                [LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Cneq,
                    [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [ARITH (Aadd,
                      [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (32, HEX 1)]);
                     VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None,
                  [IF ("",
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Cneq,
                        [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 0)])]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (32, HEX 1);
                         VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None, [])])])])]);
             BGN (None, [])]);
           IF ("",
            [CMP (Ceq,
              [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("SIZE", (BASDTYP, "logic", TYPNONE, []), [])]);
             BGN (None,
              [BGN (None, []);
               ASGN (false, "",
                [CNST (1, STRING "1");
                 VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
             BGN (None,
              [IF ("",
                [CMP (Ceq,
                  [VRF ("iCount", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (32, HEX 0)]);
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [CNST (1, STRING "0");
                     VRF ("Q", (BASDTYP, "logic", TYPNONE, []), [])])]);
                 BGN (None, [])])])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           ASGN (false, "",
            [SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)]);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 1)])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           ASGN (false, "",
            [SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)]);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 1)])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           ASGN (false, "",
            [SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)]);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 1)])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           ASGN (false, "",
            [SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)]);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 1)])])])])]);
     ("", POSPOS ("CLK", "RST"),
      [BGN (None, []);
       IF ("",
        [CMP (Ceq,
          [VRF ("RST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [LOGIC (Lor,
              [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
               LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
             VRF ("iD", (BASDTYP, "logic", TYPNONE, []), [])])]);
         BGN (None,
          [BGN (None, []);
           ASGN (false, "",
            [VRF ("D", (BASDTYP, "logic", TYPNONE, []), []);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)])]);
           ASGN (false, "",
            [SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 0)]);
             SEL ("",
              [VRF ("iD", (BASDTYP, "logic", TYPNONE, []), []);
               CNST (32, HEX 1)])])])])]);
     ("", COMB,
      [SNTRE
        [SNITM ("CHANGED",
          [VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iMSR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iLSR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iIIR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iRBR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])]);
         SNITM ("CHANGED",
          [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []),
            [])]);
         SNITM ("CHANGED",
          [VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 0, HEX 0), []), [])])];
       BGN (None, []);
       CS ("",
        [VRF ("PADDR", (BASDTYP, "logic", TYPNONE, []), []);
         CSITM ("",
          [CNST (3, STRING "000");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iRBR", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iDLL", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])])])]);
         CSITM ("",
          [CNST (3, STRING "001");
           BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iIER", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [VRF ("iDLM", (BASDTYP, "logic", TYPNONE, []), []);
                   IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])])])]);
         CSITM ("",
          [CNST (3, STRING "010");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iIIR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "011");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iLCR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "100");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iMCR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "101");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iLSR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "110");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iMSR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [CNST (3, STRING "111");
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iSCR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])]);
         CSITM ("",
          [UNKNOWN;
           BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("iRBR", (BASDTYP, "logic", TYPNONE, []), []);
               IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "1");
               VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBaudtick16x", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "0")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iMCR_OUT1", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iMCR_OUT2", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iMCR_DTR", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iSOUT", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "0")]);
                 LOGIC (Lredand,
                  [CMP (Ceq,
                    [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Ceq,
                    [VRF ("iRXFIFOTrigger", (BASDTYP, "logic", TYPNONE, []),
                      []);
                     CNST (1, STRING "1")])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "0");
                   VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     LOGIC (Lor,
                      [CMP (Ceq,
                        [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (1, STRING "0")]);
                       LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (1, STRING "1")]);
                         CMP (Ceq,
                          [VRF ("iRXFIFOEmpty",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "1")])])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iRXFIFOD", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
             CS ("",
              [VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), []);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CAT ("",
                          [VRF ("iRXBI", (BASDTYP, "logic", TYPNONE, []), []);
                           CAT ("",
                            [VRF ("iRXFE", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CAT ("",
                              [VRF ("iRXPE", (BASDTYP, "logic", TYPNONE, []),
                                []);
                               VRF ("iRXData",
                                (BASDTYP, "logic", TYPNONE, []), [])])])]);
                         VRF ("iRXFIFOD", (BASDTYP, "logic", TYPNONE, []),
                          [])]);
                       IF ("",
                        [CMP (Ceq,
                          [VRF ("iFCR_FIFOEnable",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         BGN (None,
                          [ASGN (false, "",
                            [CNST (1, STRING "1");
                             VRF ("iRXFIFOClear",
                              (BASDTYP, "logic", TYPNONE, []), [])])]);
                         BGN (None, [])]);
                       ASGN (false, "",
                        [VRF ("RXSAVE", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iFCR_FIFOEnable",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     BGN (None,
                      [ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("iRXFIFOWrite",
                          (BASDTYP, "logic", TYPNONE, []), [])])]);
                     BGN (None,
                      [IF ("",
                        [CMP (Ceq,
                          [VRF ("iRXFIFOFull",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         BGN (None,
                          [ASGN (false, "",
                            [CNST (1, STRING "1");
                             VRF ("iRXFIFOWrite",
                              (BASDTYP, "logic", TYPNONE, []), [])])]);
                         BGN (None, [])])])]);
                   ASGN (false, "",
                    [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               CSITM ("",
                [UNKNOWN;
                 ASGN (false, "",
                  [VRF ("RXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iTSR", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
             CS ("",
              [VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), []);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iTXEnable", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     BGN (None,
                      [BGN (None, []);
                       ASGN (false, "",
                        [CNST (1, STRING "1");
                         VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []),
                          [])]);
                       ASGN (false, "",
                        [VRF ("TXSTART", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   ASGN (false, "",
                    [VRF ("iTXFIFOQ", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("iTSR", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [VRF ("TXRUN", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 BGN (None,
                  [BGN (None, []);
                   IF ("",
                    [CMP (Ceq,
                      [VRF ("iTXFinished", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("TXEND", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])]);
                     BGN (None,
                      [ASGN (false, "",
                        [VRF ("TXRUN", (BASDTYP, "logic", TYPNONE, []), []);
                         VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []),
                          [])])])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
                   ASGN (false, "",
                    [CNST (1, STRING "1");
                     VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])])])]);
               CSITM ("",
                [CNST (2, STRING "3");
                 ASGN (false, "",
                  [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])])]);
               CSITM ("",
                [UNKNOWN;
                 ASGN (false, "",
                  [VRF ("TXIDLE", (BASDTYP, "logic", TYPNONE, []), []);
                   VRF ("tx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iSCR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iSCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iSCR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iCTSnRE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iCTSnFE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iDSRnRE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iDSRnFE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iRInFE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [LOGIC (Lor,
                [CMP (Ceq,
                  [VRF ("iDCDnRE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")]);
                 CMP (Ceq,
                  [VRF ("iDCDnFE", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (32, HEX 0);
               VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iFCR_FIFOEnable",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "0")]);
                     CMP (Ceq,
                      [VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])]);
                 LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iFCR_FIFOEnable",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iPERE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iFERE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iBIRE", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [CMP (Ceq,
                    [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Cneq,
                [VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (32, HEX 0)]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lor,
                    [CMP (Ceq,
                      [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [IRNG ("", [CNST (32, HEX 10); CNST (32, HEX 8)]);
                       CNST (3, STRING "000")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (32, HEX 0);
                   VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       CNST (1, STRING "0")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (32, HEX 1)]);
                       VRF ("iFECounter", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None,
                    [IF ("",
                      [LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iFEIncrement",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         CMP (Ceq,
                          [VRF ("iFEDecrement",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "1")])]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [CNST (32, HEX 1);
                           VRF ("iFECounter",
                            (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None, [])])])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               IRNG ("", [CNST (32, HEX 5); CNST (32, HEX 0)])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iMCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 5); CNST (32, HEX 0)]);
                   IRNG ("", [CNST (32, HEX 5); CNST (32, HEX 0)])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iLCR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iLCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iLCR", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iFCR_RXTrigger", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iFCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [SEL ("",
                    [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (32, HEX 0)]);
                   VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                    [])]);
                 ASGN (false, "",
                  [SEL ("",
                    [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (32, HEX 3)]);
                   VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 6)]);
                   VRF ("iFCR_RXTrigger", (BASDTYP, "logic", TYPNONE, []),
                    [])]);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [SEL ("",
                        [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 5)]);
                       VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])]);
                 IF ("",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 1)]);
                         CNST (1, STRING "1")]);
                       LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iFCR_FIFOEnable",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         CMP (Ceq,
                          [SEL ("",
                            [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CNST (32, HEX 0)]);
                           CNST (1, STRING "1")])])]);
                     LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iFCR_FIFOEnable",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 0)]);
                         CNST (1, STRING "0")])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iFCR_RXFIFOReset",
                        (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])]);
                 IF ("",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 2)]);
                         CNST (1, STRING "1")]);
                       LOGIC (Lredand,
                        [CMP (Ceq,
                          [VRF ("iFCR_FIFOEnable",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (1, STRING "0")]);
                         CMP (Ceq,
                          [SEL ("",
                            [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                              []);
                             CNST (32, HEX 0)]);
                           CNST (1, STRING "1")])])]);
                     LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iFCR_FIFOEnable",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "1")]);
                       CMP (Ceq,
                        [SEL ("",
                          [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []),
                            []);
                           CNST (32, HEX 0)]);
                         CNST (1, STRING "0")])])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "1");
                       VRF ("iFCR_TXFIFOReset",
                        (BASDTYP, "logic", TYPNONE, []), [])])]);
                   BGN (None, [])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 5)]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iTimeoutCount", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [CMP (Ceq,
                    [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                      []);
                     CNST (1, STRING "1")]);
                   CMP (Ceq,
                    [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])]);
                 CMP (Ceq,
                  [VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), []);
                   CNST (1, STRING "1")])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 5)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
                   VRF ("iTimeoutCount", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lredand,
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iRXFIFOEmpty",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (1, STRING "0")]);
                       CMP (Ceq,
                        [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (1, STRING "1")])]);
                     CMP (Ceq,
                      [SEL ("",
                        [VRF ("iTimeoutCount",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 5)]);
                       CNST (1, STRING "0")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [ARITH (Aadd,
                        [VRF ("iTimeoutCount",
                          (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 1)]);
                       VRF ("iTimeoutCount", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 IF ("",
                  [CMP (Ceq,
                    [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None,
                    [IF ("",
                      [CMP (Ceq,
                        [SEL ("",
                          [VRF ("iTimeoutCount",
                            (BASDTYP, "logic", TYPNONE, []), []);
                           CNST (32, HEX 5)]);
                         CNST (1, STRING "1")]);
                       BGN (None,
                        [BGN (None, []);
                         ASGN (false, "",
                          [CNST (1, STRING "1");
                           VRF ("iCharTimeout",
                            (BASDTYP, "logic", TYPNONE, []), [])])]);
                       BGN (None, [])])])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "0");
                   VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (1, STRING "0");
               VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [CMP (Ceq,
                    [VRF ("iLSR_THRERE", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")]);
                   CMP (Ceq,
                    [VRF ("iFCR_TXFIFOReset",
                      (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])]);
                 LOGIC (Lredand,
                  [LOGIC (Lredand,
                    [CMP (Ceq,
                      [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")]);
                     CMP (Ceq,
                      [SEL ("",
                        [VRF ("PWDATA", (BASDTYP, "logic", TYPNONE, []), []);
                         CNST (32, HEX 1)]);
                       CNST (1, STRING "1")])]);
                   CMP (Ceq,
                    [VRF ("iLSR_THRE", (BASDTYP, "logic", TYPNONE, []), []);
                     CNST (1, STRING "1")])])]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [CNST (1, STRING "1");
                   VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None,
                [IF ("",
                  [LOGIC (Lor,
                    [LOGIC (Lredand,
                      [CMP (Ceq,
                        [VRF ("iIIRRead", (BASDTYP, "logic", TYPNONE, []),
                          []);
                         CNST (1, STRING "1")]);
                       CMP (Ceq,
                        [IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 1)]);
                         CNST (3, STRING "001")])]);
                     CMP (Ceq,
                      [VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), []);
                       CNST (1, STRING "1")])]);
                   BGN (None,
                    [BGN (None, []);
                     ASGN (false, "",
                      [CNST (1, STRING "0");
                       VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []),
                        [])])]);
                   BGN (None, [])])])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [CNST (32, HEX 0);
               IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 0)])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 0)]);
                   IRNG ("", [CNST (32, HEX 3); CNST (32, HEX 0)])])]);
               BGN (None, [])])])])])]);
     ("", POSPOS ("CLK", "iRST"),
      [IF ("",
        [CMP (Ceq,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
           CNST (1, STRING "1")]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iDLL", (BASDTYP, "logic", TYPNONE, []), [])]);
             ASGN (false, "",
              [LOGIC (Lor,
                [LOGIC (Lor,
                  [LOGIC (Lor,
                    [LOGIC (Lor,
                      [LOGIC (Lor,
                        [LOGIC (Lor,
                          [LOGIC (Lor,
                            [LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 7)]);
                             LOGIC (Lshiftl,
                              [CNST (32, HEX 0); CNST (32, HEX 6)])]);
                           LOGIC (Lshiftl,
                            [CNST (32, HEX 0); CNST (32, HEX 5)])]);
                         LOGIC (Lshiftl,
                          [CNST (32, HEX 0); CNST (32, HEX 4)])]);
                       LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 3)])]);
                     LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 2)])]);
                   LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 1)])]);
                 LOGIC (Lshiftl, [CNST (32, HEX 0); CNST (32, HEX 0)])]);
               VRF ("iDLM", (BASDTYP, "logic", TYPNONE, []), [])])])]);
         BGN (None,
          [BGN (None,
            [BGN (None, []);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDLLWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iDLL", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])]);
             IF ("",
              [CMP (Ceq,
                [VRF ("iDLMWrite", (BASDTYP, "logic", TYPNONE, []), []);
                 CNST (1, STRING "1")]);
               BGN (None,
                [BGN (None, []);
                 ASGN (false, "",
                  [IRNG ("", [CNST (32, HEX 7); CNST (32, HEX 0)]);
                   VRF ("iDLM", (BASDTYP, "logic", TYPNONE, []), [])])]);
               BGN (None, [])])])])])])]};
 init = {contents = []}; func = {contents = []}; task = {contents = []};
 gen = {contents = []}; imp = {contents = []}; inst = {contents = []};
 cnst = {contents = []}; needed = {contents = []}; remove_interfaces = false;
 names'' = []}
