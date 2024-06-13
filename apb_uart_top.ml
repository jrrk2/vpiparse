open Input
open Input_dump

let apb_uart_dump () = dump' ("apb_uart", 
 (Work, {io =
   {contents =
     [("CLK", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("RSTN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("PSEL", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("PENABLE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("PWRITE", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("PADDR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), Dinput, "logic",
        []));
      ("PWDATA",
       ("", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), Dinput, "logic",
        []));
      ("PRDATA",
       ("", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), Doutput, "logic",
        []));
      ("PREADY",
       ("", (BASDTYP, "wire", TYPNONE, []), Doutput, "logic", [(32, SHEX 1)]));
      ("PSLVERR",
       ("", (BASDTYP, "wire", TYPNONE, []), Doutput, "logic", [(32, SHEX 1)]));
      ("INT", ("", (BASDTYP, "logic", TYPNONE, []), Doutput, "logic", []));
      ("OUT1N", ("", (BASDTYP, "logic", TYPNONE, []), Doutput, "logic", []));
      ("OUT2N", ("", (BASDTYP, "logic", TYPNONE, []), Doutput, "logic", []));
      ("RTSN", ("", (BASDTYP, "logic", TYPNONE, []), Doutput, "logic", []));
      ("DTRN", ("", (BASDTYP, "logic", TYPNONE, []), Doutput, "logic", []));
      ("CTSN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("DSRN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("DCDN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("RIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("SIN", ("", (BASDTYP, "logic", TYPNONE, []), Dinput, "logic", []));
      ("SOUT", ("", (BASDTYP, "logic", TYPNONE, []), Doutput, "logic", []))]};
  v =
   {contents =
     [("iWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRead",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRST",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRBRRead",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTHRWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDLLWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDLMWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iIERWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iIIRRead",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFCRWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCRWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCRWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSRRead",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSRRead",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iSCRWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTSR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRBR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDLL",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDLM",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iIER",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iIIR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iSCR",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFCR_FIFOEnable",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFCR_RXFIFOReset",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFCR_TXFIFOReset",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFCR_DMAMode",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFCR_FIFO64E",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFCR_RXTrigger",
       ("", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR_WLS",
       ("", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR_STB",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR_PEN",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR_EPS",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR_SP",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR_BC",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLCR_DLAB",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCR_DTR",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCR_RTS",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCR_OUT1",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCR_OUT2",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCR_LOOP",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMCR_AFE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_DR",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_OE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_PE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_FE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_BI",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_THRNF",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_FIFOERR",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_dCTS",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_dDSR",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_TERI",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_dDCD",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_CTS",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_DSR",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_RI",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iMSR_DCD",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iCTSNs",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDSRNs",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDCDNs",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRINs",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iCTSn",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDSRn",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDCDn",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRIn",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iCTSnRE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iCTSnFE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDSRnRE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDSRnFE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDCDnRE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iDCDnFE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRInRE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRInFE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iBaudgenDiv",
       ("", (BASDTYP, "logic", TYPRNG (HEX 15, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iBaudtick16x",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iBaudtick2x",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRCLK",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iBAUDOUTN",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXFIFOWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXFIFORead",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXFIFOEmpty",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXFIFO64Full",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXFIFOUsage",
       ("", (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXFIFOQ",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOClear",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOWrite",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOEmpty",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOFull",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFO16Full",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFO64Full",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOD",
       ("", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOQ",
       ("", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOUsage",
       ("", (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOTrigger",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOPE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOFE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFIFOBI",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iSOUT",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXStart",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXFinished",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXRunning",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iSINr",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iSIN",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFinished",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXData",
       ("", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXPE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXFE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRXBI",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFERE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iPERE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iBIRE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFECounter",
       ("", (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFEIncrement",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iFEDecrement",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRDAInterrupt",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTimeoutCount",
       ("", (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iCharTimeout",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iLSR_THRERE",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTHRInterrupt",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iTXEnable",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("iRTS",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("rx_State",
       ("", (BASDTYP, "logic", TYPNONE, []), "rx_state_type",
        (UNKDTYP, "", TYPNONE, [])));
      ("tx_State",
       ("", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), "tx_state_type",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_0",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_1",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_2",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_3",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_4",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_5",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_6",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_7",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_8",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_9",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_10",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_11",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_12",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_13",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_14",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])));
      ("__VdfgRegularize_ha9a614e7_1_15",
       ("", (BASDTYP, "logic", TYPNONE, []), "logic",
        (UNKDTYP, "", TYPNONE, [])))]};
  iv =
   {contents =
     [("iTXClear",
       ("", (BASDTYP, "logic", TYPNONE, []), [CNST (32, SHEX 1)], 1));
      ("iRXClear",
       ("", (BASDTYP, "logic", TYPNONE, []), [CNST (32, SHEX 1)], 1))]};
  ir = {contents = []};
  ca =
   {contents =
     [("", VRF ("PREADY", (BASDTYP, "logic", TYPNONE, []), []),
       CNST (32, SHEX 1));
      ("", VRF ("PSLVERR", (BASDTYP, "logic", TYPNONE, []), []),
       CNST (32, SHEX 1));
      ("", VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []),
       UNRY (Unot, [VRF ("RSTN", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iDLLWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_2",
          (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iDLMWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_3",
          (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_1",
          (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("__VdfgRegularize_ha9a614e7_1_3",
          (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iLSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []),
       CAT ("",
        [LOGIC (Land,
          [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])]);
         CAT ("",
          [LOGIC (Land,
            [UNRY (Unot,
              [VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
             VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
           CAT ("",
            [VRF ("iLSR_THRNF", (BASDTYP, "logic", TYPNONE, []), []);
             CAT ("",
              [VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), []);
               CAT ("",
                [VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), []);
                 CAT ("",
                  [VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), []);
                   CAT ("",
                    [VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]));
      ("", VRF ("iRDAInterrupt", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Lor,
        [LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_5",
            (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), [])]);
         LOGIC (Land,
          [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iRXFIFOTrigger", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("", VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iMSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []),
       CAT ("",
        [VRF ("iMSR_DCD", (BASDTYP, "logic", TYPNONE, []), []);
         CAT ("",
          [VRF ("iMSR_RI", (BASDTYP, "logic", TYPNONE, []), []);
           CAT ("",
            [VRF ("iMSR_DSR", (BASDTYP, "logic", TYPNONE, []), []);
             CAT ("",
              [VRF ("iMSR_CTS", (BASDTYP, "logic", TYPNONE, []), []);
               CAT ("",
                [VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), []);
                 CAT ("",
                  [VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), []);
                   CAT ("",
                    [VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), []);
                     VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]));
      ("",
       SEL ("",
        [VRF ("iIIR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]),
       CAT ("",
        [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
         CAT ("",
          [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
           CAT ("",
            [LOGIC (Land,
              [VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
             CNST (32, SHEX 1)])])]));
      ("", VRF ("iIIRRead", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iRead", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("__VdfgRegularize_ha9a614e7_1_4",
          (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_1",
          (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("__VdfgRegularize_ha9a614e7_1_2",
          (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iRead", (BASDTYP, "logic", TYPNONE, []), []);
         LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_1",
            (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("__VdfgRegularize_ha9a614e7_1_0",
            (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("", VRF ("iFCRWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iWrite", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("__VdfgRegularize_ha9a614e7_1_4",
          (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iLCRWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iWrite", (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Ceq,
          [CNST (32, SHEX 3);
           VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])])]));
      ("",
       SEL ("",
        [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]),
       CNST (32, SHEX 2));
      ("", VRF ("iMCRWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iWrite", (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Ceq,
          [CNST (32, SHEX 3);
           VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])])]));
      ("", VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Lor,
        [VRF ("__VdfgRegularize_ha9a614e7_1_6",
          (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []), []),
       CND ("",
        [VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iRXFIFO64Full", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iRXFIFO16Full", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iRead", (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Ceq,
          [CNST (32, SHEX 3);
           VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])])]));
      ("", VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Cneq,
          [CNST (32, SHEX 3);
           SEL ("",
            [VRF ("iRXFIFOD", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []),
              []);
             CNST (32, SHEX 32); CNST (32, SHEX 32)])])]));
      ("", VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [CMP (Cneq,
          [CNST (32, SHEX 7);
           VRF ("iFECounter", (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []),
            [])]);
         LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_6",
            (BASDTYP, "logic", TYPNONE, []), []);
           LOGIC (Lor,
            [VRF ("iPERE", (BASDTYP, "logic", TYPNONE, []), []);
             LOGIC (Lor,
              [VRF ("iBIRE", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iFERE", (BASDTYP, "logic", TYPNONE, []), [])])])])]));
      ("", VRF ("iRXFIFOPE", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_6",
          (BASDTYP, "logic", TYPNONE, []), []);
         SEL ("",
          [VRF ("iRXFIFOQ", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []),
            []);
           CNST (32, SHEX 32); CNST (32, SHEX 32)])]));
      ("", VRF ("iRXFIFOFE", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_6",
          (BASDTYP, "logic", TYPNONE, []), []);
         SEL ("",
          [VRF ("iRXFIFOQ", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []),
            []);
           CNST (32, SHEX 32); CNST (32, SHEX 32)])]));
      ("", VRF ("iRXFIFOBI", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_6",
          (BASDTYP, "logic", TYPNONE, []), []);
         SEL ("",
          [VRF ("iRXFIFOQ", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []),
            []);
           CNST (32, SHEX 32); CNST (32, SHEX 32)])]));
      ("", VRF ("iMSR_CTS", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Lor,
        [LOGIC (Land,
          [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])]);
         LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_7",
            (BASDTYP, "logic", TYPNONE, []), []);
           UNRY (Unot, [VRF ("iCTSn", (BASDTYP, "logic", TYPNONE, []), [])])])]));
      ("", VRF ("iMSR_DSR", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Lor,
        [LOGIC (Land,
          [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iMCR_DTR", (BASDTYP, "logic", TYPNONE, []), [])]);
         LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_7",
            (BASDTYP, "logic", TYPNONE, []), []);
           UNRY (Unot, [VRF ("iDSRn", (BASDTYP, "logic", TYPNONE, []), [])])])]));
      ("", VRF ("iMSR_RI", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Lor,
        [LOGIC (Land,
          [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iMCR_OUT1", (BASDTYP, "logic", TYPNONE, []), [])]);
         LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_7",
            (BASDTYP, "logic", TYPNONE, []), []);
           UNRY (Unot, [VRF ("iRIn", (BASDTYP, "logic", TYPNONE, []), [])])])]));
      ("", VRF ("iMSR_DCD", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Lor,
        [LOGIC (Land,
          [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iMCR_OUT2", (BASDTYP, "logic", TYPNONE, []), [])]);
         LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_7",
            (BASDTYP, "logic", TYPNONE, []), []);
           UNRY (Unot, [VRF ("iDCDn", (BASDTYP, "logic", TYPNONE, []), [])])])]));
      ("", VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iRead", (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Ceq,
          [CNST (32, SHEX 3);
           VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])])]));
      ("", VRF ("iSCRWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iWrite", (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Ceq,
          [CNST (32, SHEX 3);
           VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])])]));
      ("", VRF ("iTXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iLSR_THRNF", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("",
       VRF ("iLCR_WLS", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), []),
       SEL ("",
        [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iLCR_STB", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iLCR_PEN", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iLCR_EPS", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iLCR_SP", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iLCR_BC", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iSIN", (BASDTYP, "logic", TYPNONE, []), []),
       CND ("",
        [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iSOUT", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("iSINr", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iTXEnable", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [UNRY (Unot,
          [VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
         LOGIC (Lor,
          [UNRY (Unot,
            [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), [])]);
           LOGIC (Land,
            [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
             VRF ("iMSR_CTS", (BASDTYP, "logic", TYPNONE, []), [])])])]));
      ("", VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iRXFIFOTrigger", (BASDTYP, "logic", TYPNONE, []), []),
       CND ("",
        [VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), []);
         LOGIC (Lor,
          [VRF ("__VdfgRegularize_ha9a614e7_1_14",
            (BASDTYP, "logic", TYPNONE, []), []);
           LOGIC (Lor,
            [LOGIC (Land,
              [VRF ("__VdfgRegularize_ha9a614e7_1_8",
                (BASDTYP, "logic", TYPNONE, []), []);
               LOGIC (Lor,
                [VRF ("iRXFIFO16Full", (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("__VdfgRegularize_ha9a614e7_1_13",
                  (BASDTYP, "logic", TYPNONE, []), [])])]);
             LOGIC (Lor,
              [LOGIC (Land,
                [VRF ("__VdfgRegularize_ha9a614e7_1_11",
                  (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("__VdfgRegularize_ha9a614e7_1_13",
                  (BASDTYP, "logic", TYPNONE, []), [])]);
               LOGIC (Lor,
                [LOGIC (Land,
                  [VRF ("__VdfgRegularize_ha9a614e7_1_12",
                    (BASDTYP, "logic", TYPNONE, []), []);
                   LOGIC (Land,
                    [VRF ("__VdfgRegularize_ha9a614e7_1_13",
                      (BASDTYP, "logic", TYPNONE, []), []);
                     LOGIC (Land,
                      [VRF ("iRXFIFO16Full", (BASDTYP, "logic", TYPNONE, []),
                        []);
                       VRF ("__VdfgRegularize_ha9a614e7_1_10",
                        (BASDTYP, "logic", TYPNONE, []), [])])])]);
                 VRF ("iRXFIFO64Full", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
         LOGIC (Lor,
          [VRF ("__VdfgRegularize_ha9a614e7_1_14",
            (BASDTYP, "logic", TYPNONE, []), []);
           LOGIC (Lor,
            [LOGIC (Land,
              [VRF ("__VdfgRegularize_ha9a614e7_1_8",
                (BASDTYP, "logic", TYPNONE, []), []);
               LOGIC (Lor,
                [VRF ("__VdfgRegularize_ha9a614e7_1_9",
                  (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("__VdfgRegularize_ha9a614e7_1_10",
                  (BASDTYP, "logic", TYPNONE, []), [])])]);
             LOGIC (Lor,
              [LOGIC (Land,
                [VRF ("__VdfgRegularize_ha9a614e7_1_11",
                  (BASDTYP, "logic", TYPNONE, []), []);
                 VRF ("__VdfgRegularize_ha9a614e7_1_10",
                  (BASDTYP, "logic", TYPNONE, []), [])]);
               LOGIC (Lor,
                [LOGIC (Land,
                  [VRF ("__VdfgRegularize_ha9a614e7_1_12",
                    (BASDTYP, "logic", TYPNONE, []), []);
                   LOGIC (Land,
                    [VRF ("__VdfgRegularize_ha9a614e7_1_10",
                      (BASDTYP, "logic", TYPNONE, []), []);
                     LOGIC (Land,
                      [VRF ("__VdfgRegularize_ha9a614e7_1_9",
                        (BASDTYP, "logic", TYPNONE, []), []);
                       SEL ("",
                        [VRF ("iRXFIFOUsage",
                          (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                          []);
                         CNST (32, SHEX 32); CNST (32, SHEX 32)])])])]);
                 VRF ("iRXFIFO16Full", (BASDTYP, "logic", TYPNONE, []), [])])])])])]));
      ("", VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iMCR_OUT1", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iMCR_OUT2", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iMCR_DTR", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("", VRF ("iRBR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []),
       SEL ("",
        [VRF ("iRXFIFOQ", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_15",
        (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("PENABLE", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("PSEL", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iWrite", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_15",
          (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("PWRITE", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("", VRF ("iRead", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [UNRY (Unot, [VRF ("PWRITE", (BASDTYP, "logic", TYPNONE, []), [])]);
         VRF ("__VdfgRegularize_ha9a614e7_1_15",
          (BASDTYP, "logic", TYPNONE, []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_1",
        (BASDTYP, "logic", TYPNONE, []), []),
       UNRY (Unot, [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_0",
        (BASDTYP, "logic", TYPNONE, []), []),
       CMP (Ceq,
        [CNST (32, SHEX 3);
         VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_2",
        (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iWrite", (BASDTYP, "logic", TYPNONE, []), []);
         VRF ("__VdfgRegularize_ha9a614e7_1_0",
          (BASDTYP, "logic", TYPNONE, []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_3",
        (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("iWrite", (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Ceq,
          [CNST (32, SHEX 3);
           VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_4",
        (BASDTYP, "logic", TYPNONE, []), []),
       CMP (Ceq,
        [CNST (32, SHEX 3);
         VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])]));
      ("", VRF ("iLSR_THRNF", (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Lor,
        [LOGIC (Land,
          [VRF ("__VdfgRegularize_ha9a614e7_1_5",
            (BASDTYP, "logic", TYPNONE, []), []);
           VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
         LOGIC (Land,
          [UNRY (Unot,
            [CND ("",
              [VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), []);
               VRF ("iTXFIFO64Full", (BASDTYP, "logic", TYPNONE, []), []);
               SEL ("",
                [VRF ("iTXFIFOUsage",
                  (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), []);
                 CNST (32, SHEX 32); CNST (32, SHEX 32)])])]);
           VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_5",
        (BASDTYP, "logic", TYPNONE, []), []),
       UNRY (Unot,
        [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_6",
        (BASDTYP, "logic", TYPNONE, []), []),
       UNRY (Unot,
        [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_14",
        (BASDTYP, "logic", TYPNONE, []), []),
       LOGIC (Land,
        [VRF ("__VdfgRegularize_ha9a614e7_1_6",
          (BASDTYP, "logic", TYPNONE, []), []);
         CMP (Ceq,
          [CNST (32, SHEX 2);
           VRF ("iFCR_RXTrigger",
            (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_8",
        (BASDTYP, "logic", TYPNONE, []), []),
       CMP (Ceq,
        [CNST (32, SHEX 2);
         VRF ("iFCR_RXTrigger",
          (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])]));
      ("", VRF ("iRXFIFO16Full", (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iRXFIFOUsage", (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []),
          []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_13",
        (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iRXFIFOUsage", (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []),
          []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_11",
        (BASDTYP, "logic", TYPNONE, []), []),
       CMP (Ceq,
        [CNST (32, SHEX 2);
         VRF ("iFCR_RXTrigger",
          (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_12",
        (BASDTYP, "logic", TYPNONE, []), []),
       CMP (Ceq,
        [CNST (32, SHEX 2);
         VRF ("iFCR_RXTrigger",
          (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_10",
        (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iRXFIFOUsage", (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []),
          []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_9",
        (BASDTYP, "logic", TYPNONE, []), []),
       SEL ("",
        [VRF ("iRXFIFOUsage", (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []),
          []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]));
      ("",
       VRF ("__VdfgRegularize_ha9a614e7_1_7",
        (BASDTYP, "logic", TYPNONE, []), []),
       UNRY (Unot, [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), [])]));
      ("",
       SEL ("",
        [VRF ("PRDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]),
       CNST (32, SHEX 24));
      ("",
       VRF ("iBaudgenDiv", (BASDTYP, "logic", TYPRNG (HEX 15, HEX 0), []),
        []),
       CAT ("",
        [VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]));
      ("",
       SEL ("",
        [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
         CNST (32, SHEX 32); CNST (32, SHEX 32)]),
       CNST (32, SHEX 4))]};
  alwys =
   {contents =
     [("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])]);
          BGN (None,
           [IF ("",
             [VRF ("iDLLWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   [])])])]);
            IF ("",
             [VRF ("iDLMWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   [])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 4);
              SEL ("",
               [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                 []);
                CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
          BGN (None,
           [IF ("",
             [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  SEL ("",
                   [VRF ("iIER",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
                    CNST (32, SHEX 3); CNST (32, SHEX 32)])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])]);
          BGN (None,
           [IF ("",
             [LOGIC (Lor,
               [LOGIC (Lor,
                 [VRF ("iLSR_THRERE", (BASDTYP, "logic", TYPNONE, []), []);
                  VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []),
                   [])]);
                LOGIC (Land,
                 [LOGIC (Land,
                   [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
                    SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)])]);
                  VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Lor,
                   [LOGIC (Land,
                     [VRF ("iIIRRead", (BASDTYP, "logic", TYPNONE, []), []);
                      CMP (Ceq,
                       [CNST (32, SHEX 3);
                        SEL ("",
                         [VRF ("iIIR",
                           (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), 
                           []);
                          CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
                    VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []),
                       [])])])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 6);
              VRF ("iTimeoutCount",
               (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])]);
          BGN (None,
           [IF ("",
             [LOGIC (Lor,
               [LOGIC (Lor,
                 [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), []);
                  VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), [])]);
                VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 6);
                  VRF ("iTimeoutCount",
                   (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Land,
                   [LOGIC (Land,
                     [UNRY (Unot,
                       [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                         [])]);
                      VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []),
                       [])]);
                    UNRY (Unot,
                     [SEL ("",
                       [VRF ("iTimeoutCount",
                         (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                         []);
                        CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [ARITH (Aadd,
                       [CNST (32, SHEX 6);
                        VRF ("iTimeoutCount",
                         (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                         [])]);
                      VRF ("iTimeoutCount",
                       (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                       [])])])])])]);
            IF ("",
             [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [IF ("",
                 [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []),
                       [])])]);
                  BGN (None,
                   [IF ("",
                     [SEL ("",
                       [VRF ("iTimeoutCount",
                         (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                         []);
                        CNST (32, SHEX 3); CNST (32, SHEX 32)]);
                      BGN (None,
                       [ASGN (true, "",
                         [CNST (32, SHEX 1);
                          VRF ("iCharTimeout",
                           (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 2);
              VRF ("iFCR_RXTrigger",
               (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])]);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            IF ("",
             [VRF ("iFCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                   [])]);
                ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
                ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iFCR_RXTrigger",
                   (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])]);
                IF ("",
                 [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [SEL ("",
                       [VRF ("PWDATA",
                         (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                         []);
                        CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                      VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []),
                       [])])])]);
                IF ("",
                 [LOGIC (Lor,
                   [LOGIC (Lor,
                     [SEL ("",
                       [VRF ("PWDATA",
                         (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                         []);
                        CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                      LOGIC (Land,
                       [UNRY (Unot,
                         [VRF ("iFCR_FIFOEnable",
                           (BASDTYP, "logic", TYPNONE, []), [])]);
                        SEL ("",
                         [VRF ("PWDATA",
                           (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                           []);
                          CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
                    LOGIC (Land,
                     [VRF ("iFCR_FIFOEnable",
                       (BASDTYP, "logic", TYPNONE, []), []);
                      UNRY (Unot,
                       [SEL ("",
                         [VRF ("PWDATA",
                           (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                           []);
                          CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iFCR_RXFIFOReset",
                       (BASDTYP, "logic", TYPNONE, []), [])])])]);
                IF ("",
                 [LOGIC (Lor,
                   [LOGIC (Lor,
                     [SEL ("",
                       [VRF ("PWDATA",
                         (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                         []);
                        CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                      LOGIC (Land,
                       [UNRY (Unot,
                         [VRF ("iFCR_FIFOEnable",
                           (BASDTYP, "logic", TYPNONE, []), [])]);
                        SEL ("",
                         [VRF ("PWDATA",
                           (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                           []);
                          CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
                    LOGIC (Land,
                     [VRF ("iFCR_FIFOEnable",
                       (BASDTYP, "logic", TYPNONE, []), []);
                      UNRY (Unot,
                       [SEL ("",
                         [VRF ("PWDATA",
                           (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                           []);
                          CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iFCR_TXFIFOReset",
                       (BASDTYP, "logic", TYPNONE, []), [])])])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])]);
          BGN (None,
           [IF ("",
             [VRF ("iLCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   [])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 6);
              SEL ("",
               [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                 []);
                CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
          BGN (None,
           [IF ("",
             [VRF ("iMCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  SEL ("",
                   [VRF ("iMCR",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
                    CNST (32, SHEX 3); CNST (32, SHEX 32)])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 7);
              VRF ("iFECounter",
               (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])]);
          BGN (None,
           [IF ("",
             [LOGIC (Land,
               [LOGIC (Lor,
                 [LOGIC (Land,
                   [UNRY (Unot,
                     [VRF ("iFCR_FIFOEnable",
                       (BASDTYP, "logic", TYPNONE, []), [])]);
                    VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), [])]);
                  LOGIC (Land,
                   [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                     []);
                    VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []), [])])]);
                VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iPERE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iFERE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iBIRE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [CMP (Cneq,
               [CNST (32, SHEX 7);
                VRF ("iFECounter",
                 (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Lor,
                   [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), []);
                    CMP (Ceq,
                     [CNST (32, SHEX 3);
                      SEL ("",
                       [VRF ("iRXFIFOQ",
                         (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), 
                         []);
                        CNST (32, SHEX 4); CNST (32, SHEX 32)])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []),
                       [])])])])])]);
            IF ("",
             [VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 7);
                  VRF ("iFECounter",
                   (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Land,
                   [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), []);
                    UNRY (Unot,
                     [VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []),
                       [])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [ARITH (Aadd,
                       [CNST (32, SHEX 7);
                        VRF ("iFECounter",
                         (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), 
                         [])]);
                      VRF ("iFECounter",
                       (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), 
                       [])])]);
                  BGN (None,
                   [IF ("",
                     [LOGIC (Land,
                       [UNRY (Unot,
                         [VRF ("iFEIncrement",
                           (BASDTYP, "logic", TYPNONE, []), [])]);
                        VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []),
                         [])]);
                      BGN (None,
                       [ASGN (true, "",
                         [ARITH (Asub,
                           [VRF ("iFECounter",
                             (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []),
                             []);
                            CNST (32, SHEX 7)]);
                          VRF ("iFECounter",
                           (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), 
                           [])])])])])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
          BGN (None,
           [IF ("",
             [LOGIC (Lor,
               [VRF ("iCTSnRE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iCTSnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iDSRnRE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iDSRnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iRInFE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iDCDnRE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iDCDnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])]);
          BGN (None,
           [IF ("",
             [VRF ("iSCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   [])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 2);
              VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
               [])]);
            ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iTSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])])]);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
            CS ("",
             [VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
               []);
              CSITM ("",
               [CNST (32, SHEX 2);
                IF ("",
                 [VRF ("iTXEnable", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
                    ASGN (true, "",
                     [CNST (32, SHEX 2);
                      VRF ("tx_State",
                       (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                       [])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 2);
                      VRF ("tx_State",
                       (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                       [])])])])]);
              CSITM ("",
               [CNST (32, SHEX 2);
                BGN (None,
                 [ASGN (true, "",
                   [VRF ("iTXFIFOQ",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
                    VRF ("iTSR",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 2);
                    VRF ("tx_State",
                     (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])])]);
              CSITM ("",
               [CNST (32, SHEX 2);
                BGN (None,
                 [IF ("",
                   [VRF ("iTXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 2);
                        VRF ("tx_State",
                         (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                         [])])]);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 2);
                        VRF ("tx_State",
                         (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                         [])])])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])])])]);
              CSITM ("",
               [CNST (32, SHEX 2);
                ASGN (true, "",
                 [CNST (32, SHEX 2);
                  VRF ("tx_State",
                   (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])]);
              CSITM ("",
               [ASGN (true, "",
                 [CNST (32, SHEX 2);
                  VRF ("tx_State",
                   (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 11);
              VRF ("iRXFIFOD",
               (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), [])])]);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), []);
              VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
            CS ("",
             [VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), []);
              CSITM ("",
               [CNST (32, SHEX 1);
                IF ("",
                 [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CAT ("",
                       [VRF ("iRXBI", (BASDTYP, "logic", TYPNONE, []), []);
                        CAT ("",
                         [VRF ("iRXFE", (BASDTYP, "logic", TYPNONE, []), []);
                          CAT ("",
                           [VRF ("iRXPE", (BASDTYP, "logic", TYPNONE, []),
                             []);
                            VRF ("iRXData",
                             (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                             [])])])]);
                      VRF ("iRXFIFOD",
                       (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), 
                       [])]);
                    IF ("",
                     [UNRY (Unot,
                       [VRF ("iFCR_FIFOEnable",
                         (BASDTYP, "logic", TYPNONE, []), [])]);
                      BGN (None,
                       [ASGN (true, "",
                         [CNST (32, SHEX 1);
                          VRF ("iRXFIFOClear",
                           (BASDTYP, "logic", TYPNONE, []), [])])])]);
                    ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
              CSITM ("",
               [CNST (32, SHEX 1);
                BGN (None,
                 [IF ("",
                   [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                     []);
                    BGN (None,
                     [IF ("",
                       [UNRY (Unot,
                         [VRF ("iRXFIFOFull",
                           (BASDTYP, "logic", TYPNONE, []), [])]);
                        BGN (None,
                         [ASGN (true, "",
                           [CNST (32, SHEX 1);
                            VRF ("iRXFIFOWrite",
                             (BASDTYP, "logic", TYPNONE, []), [])])])])]);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 1);
                        VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []),
                         [])])])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
              CSITM ("",
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
          BGN (None,
           [IF ("",
             [LOGIC (Lor,
               [UNRY (Unot,
                 [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), [])]);
                LOGIC (Land,
                 [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
                  VRF ("iRXFIFOTrigger", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Land,
                   [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
                    LOGIC (Lor,
                     [UNRY (Unot,
                       [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), [])]);
                      LOGIC (Land,
                       [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
                        VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                         [])])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])])]);
      ("", POSPOS ("CLK", "iRST"),
       [IF ("",
         [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])]);
          BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])]);
            IF ("",
             [UNRY (Unot,
               [VRF ("iBaudtick16x", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iMCR_OUT1", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iMCR_OUT2", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iMCR_DTR", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iSOUT", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
      ("", COMB,
       [SNTRE
         [SNITM ("CHANGED",
           [VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iIIR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), [])]);
          SNITM ("CHANGED",
           [VRF ("iLSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iMSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iRBR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
          SNITM ("CHANGED",
           [VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])];
        CS ("",
         [VRF ("PADDR", (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), []);
          CSITM ("",
           [CNST (32, SHEX 3);
            IF ("",
             [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   []);
                  SEL ("",
                   [VRF ("PRDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
              BGN (None,
               [ASGN (true, "",
                 [VRF ("iRBR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   []);
                  SEL ("",
                   [VRF ("PRDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)])])])])]);
          CSITM ("",
           [CNST (32, SHEX 3);
            IF ("",
             [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   []);
                  SEL ("",
                   [VRF ("PRDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
              BGN (None,
               [ASGN (true, "",
                 [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   []);
                  SEL ("",
                   [VRF ("PRDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)])])])])]);
          CSITM ("",
           [CNST (32, SHEX 3);
            ASGN (true, "",
             [VRF ("iIIR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              SEL ("",
               [VRF ("PRDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          CSITM ("",
           [CNST (32, SHEX 3);
            ASGN (true, "",
             [VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              SEL ("",
               [VRF ("PRDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          CSITM ("",
           [CNST (32, SHEX 3);
            ASGN (true, "",
             [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              SEL ("",
               [VRF ("PRDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          CSITM ("",
           [CNST (32, SHEX 3);
            ASGN (true, "",
             [VRF ("iLSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              SEL ("",
               [VRF ("PRDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          CSITM ("",
           [CNST (32, SHEX 3);
            ASGN (true, "",
             [VRF ("iMSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              SEL ("",
               [VRF ("PRDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          CSITM ("",
           [CNST (32, SHEX 3);
            ASGN (true, "",
             [VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              SEL ("",
               [VRF ("PRDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          CSITM ("",
           [ASGN (true, "",
             [VRF ("iRBR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              SEL ("",
               [VRF ("PRDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])])])])]};
  init = {contents = []}; func = {contents = []}; task = {contents = []};
  gen =
   {contents =
     [("",
       [VRF ("iDLLWrite", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])])]);
      ("",
       [VRF ("iDLMWrite", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])])]);
        BGN (None,
         [BGN (None,
           [IF ("",
             [VRF ("iDLLWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   [])])])]);
            IF ("",
             [VRF ("iDLMWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   [])])])])])])]);
      ("",
       [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            SEL ("",
             [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              CNST (32, SHEX 3); CNST (32, SHEX 32)])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 4);
            SEL ("",
             [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
        BGN (None,
         [IF ("",
           [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [SEL ("",
                 [VRF ("PWDATA",
                   (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                  CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                SEL ("",
                 [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   []);
                  CNST (32, SHEX 3); CNST (32, SHEX 32)])])])])])]);
      ("",
       [LOGIC (Lor,
         [LOGIC (Land,
           [VRF ("iIIRRead", (BASDTYP, "logic", TYPNONE, []), []);
            CMP (Ceq,
             [CNST (32, SHEX 3);
              SEL ("",
               [VRF ("iIIR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                 []);
                CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
          VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [LOGIC (Lor,
           [VRF ("iLSR_THRERE", (BASDTYP, "logic", TYPNONE, []), []);
            VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
          LOGIC (Land,
           [LOGIC (Land,
             [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
              SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])]);
            VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Lor,
             [LOGIC (Land,
               [VRF ("iIIRRead", (BASDTYP, "logic", TYPNONE, []), []);
                CMP (Ceq,
                 [CNST (32, SHEX 3);
                  SEL ("",
                   [VRF ("iIIR",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
                    CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
              VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Lor,
             [LOGIC (Lor,
               [VRF ("iLSR_THRERE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
              LOGIC (Land,
               [LOGIC (Land,
                 [VRF ("iIERWrite", (BASDTYP, "logic", TYPNONE, []), []);
                  SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)])]);
                VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])])]);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])])]);
            BGN (None,
             [IF ("",
               [LOGIC (Lor,
                 [LOGIC (Land,
                   [VRF ("iIIRRead", (BASDTYP, "logic", TYPNONE, []), []);
                    CMP (Ceq,
                     [CNST (32, SHEX 3);
                      SEL ("",
                       [VRF ("iIIR",
                         (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), 
                         []);
                        CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
                  VRF ("iTHRWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
                BGN (None,
                 [ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []),
                     [])])])])])])])]);
      ("",
       [LOGIC (Land,
         [LOGIC (Land,
           [UNRY (Unot,
             [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
            VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])]);
          UNRY (Unot,
           [SEL ("",
             [VRF ("iTimeoutCount",
               (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), []);
              CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
        BGN (None,
         [ASGN (true, "",
           [ARITH (Aadd,
             [CNST (32, SHEX 6);
              VRF ("iTimeoutCount",
               (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])]);
            VRF ("iTimeoutCount",
             (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])])])]);
      ("",
       [LOGIC (Lor,
         [LOGIC (Lor,
           [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), []);
            VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), [])]);
          VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 6);
            VRF ("iTimeoutCount",
             (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Land,
             [LOGIC (Land,
               [UNRY (Unot,
                 [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
                VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])]);
              UNRY (Unot,
               [SEL ("",
                 [VRF ("iTimeoutCount",
                   (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), []);
                  CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
            BGN (None,
             [ASGN (true, "",
               [ARITH (Aadd,
                 [CNST (32, SHEX 6);
                  VRF ("iTimeoutCount",
                   (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])]);
                VRF ("iTimeoutCount",
                 (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])])])])])]);
      ("",
       [SEL ("",
         [VRF ("iTimeoutCount",
           (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), []);
          CNST (32, SHEX 3); CNST (32, SHEX 32)]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [SEL ("",
             [VRF ("iTimeoutCount",
               (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), []);
              CNST (32, SHEX 3); CNST (32, SHEX 32)]);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [IF ("",
           [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])]);
            BGN (None,
             [IF ("",
               [SEL ("",
                 [VRF ("iTimeoutCount",
                   (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), []);
                  CNST (32, SHEX 3); CNST (32, SHEX 32)]);
                BGN (None,
                 [ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 6);
              VRF ("iTimeoutCount",
               (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])]);
        BGN (None,
         [BGN (None,
           [IF ("",
             [LOGIC (Lor,
               [LOGIC (Lor,
                 [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), []);
                  VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), [])]);
                VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 6);
                  VRF ("iTimeoutCount",
                   (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Land,
                   [LOGIC (Land,
                     [UNRY (Unot,
                       [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                         [])]);
                      VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []),
                       [])]);
                    UNRY (Unot,
                     [SEL ("",
                       [VRF ("iTimeoutCount",
                         (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                         []);
                        CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [ARITH (Aadd,
                       [CNST (32, SHEX 6);
                        VRF ("iTimeoutCount",
                         (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                         [])]);
                      VRF ("iTimeoutCount",
                       (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                       [])])])])])]);
            IF ("",
             [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [IF ("",
                 [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []),
                       [])])]);
                  BGN (None,
                   [IF ("",
                     [SEL ("",
                       [VRF ("iTimeoutCount",
                         (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), 
                         []);
                        CNST (32, SHEX 3); CNST (32, SHEX 32)]);
                      BGN (None,
                       [ASGN (true, "",
                         [CNST (32, SHEX 1);
                          VRF ("iCharTimeout",
                           (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
      ("",
       [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [LOGIC (Lor,
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            LOGIC (Land,
             [UNRY (Unot,
               [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
              SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          LOGIC (Land,
           [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
            UNRY (Unot,
             [SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [LOGIC (Lor,
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            LOGIC (Land,
             [UNRY (Unot,
               [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
              SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
          LOGIC (Land,
           [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
            UNRY (Unot,
             [SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iFCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)]);
              VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)]);
              VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [SEL ("",
               [VRF ("PWDATA",
                 (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                CNST (32, SHEX 5); CNST (32, SHEX 32)]);
              VRF ("iFCR_RXTrigger",
               (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])]);
            IF ("",
             [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [LOGIC (Lor,
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  LOGIC (Land,
                   [UNRY (Unot,
                     [VRF ("iFCR_FIFOEnable",
                       (BASDTYP, "logic", TYPNONE, []), [])]);
                    SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
                LOGIC (Land,
                 [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                   []);
                  UNRY (Unot,
                   [SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []),
                   [])])])]);
            IF ("",
             [LOGIC (Lor,
               [LOGIC (Lor,
                 [SEL ("",
                   [VRF ("PWDATA",
                     (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                     []);
                    CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                  LOGIC (Land,
                   [UNRY (Unot,
                     [VRF ("iFCR_FIFOEnable",
                       (BASDTYP, "logic", TYPNONE, []), [])]);
                    SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
                LOGIC (Land,
                 [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                   []);
                  UNRY (Unot,
                   [SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []),
                   [])])])])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 2);
              VRF ("iFCR_RXTrigger",
               (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])])]);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
            IF ("",
             [VRF ("iFCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [BGN (None,
                 [ASGN (true, "",
                   [SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                    VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                     [])]);
                  ASGN (true, "",
                   [SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                    VRF ("iFCR_DMAMode", (BASDTYP, "logic", TYPNONE, []), [])]);
                  ASGN (true, "",
                   [SEL ("",
                     [VRF ("PWDATA",
                       (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), 
                       []);
                      CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                    VRF ("iFCR_RXTrigger",
                     (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])]);
                  IF ("",
                   [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
                    BGN (None,
                     [ASGN (true, "",
                       [SEL ("",
                         [VRF ("PWDATA",
                           (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                           []);
                          CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                        VRF ("iFCR_FIFO64E", (BASDTYP, "logic", TYPNONE, []),
                         [])])])]);
                  IF ("",
                   [LOGIC (Lor,
                     [LOGIC (Lor,
                       [SEL ("",
                         [VRF ("PWDATA",
                           (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                           []);
                          CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                        LOGIC (Land,
                         [UNRY (Unot,
                           [VRF ("iFCR_FIFOEnable",
                             (BASDTYP, "logic", TYPNONE, []), [])]);
                          SEL ("",
                           [VRF ("PWDATA",
                             (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                             []);
                            CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
                      LOGIC (Land,
                       [VRF ("iFCR_FIFOEnable",
                         (BASDTYP, "logic", TYPNONE, []), []);
                        UNRY (Unot,
                         [SEL ("",
                           [VRF ("PWDATA",
                             (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                             []);
                            CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 1);
                        VRF ("iFCR_RXFIFOReset",
                         (BASDTYP, "logic", TYPNONE, []), [])])])]);
                  IF ("",
                   [LOGIC (Lor,
                     [LOGIC (Lor,
                       [SEL ("",
                         [VRF ("PWDATA",
                           (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                           []);
                          CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                        LOGIC (Land,
                         [UNRY (Unot,
                           [VRF ("iFCR_FIFOEnable",
                             (BASDTYP, "logic", TYPNONE, []), [])]);
                          SEL ("",
                           [VRF ("PWDATA",
                             (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                             []);
                            CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
                      LOGIC (Land,
                       [VRF ("iFCR_FIFOEnable",
                         (BASDTYP, "logic", TYPNONE, []), []);
                        UNRY (Unot,
                         [SEL ("",
                           [VRF ("PWDATA",
                             (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
                             []);
                            CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 1);
                        VRF ("iFCR_TXFIFOReset",
                         (BASDTYP, "logic", TYPNONE, []), [])])])])])])])])])]);
      ("",
       [VRF ("iLCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 8);
            VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iLCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [SEL ("",
                 [VRF ("PWDATA",
                   (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                  CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                VRF ("iLCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                 [])])])])])]);
      ("",
       [VRF ("iMCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            SEL ("",
             [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              CNST (32, SHEX 3); CNST (32, SHEX 32)])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 6);
            SEL ("",
             [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
              CNST (32, SHEX 3); CNST (32, SHEX 32)])])]);
        BGN (None,
         [IF ("",
           [VRF ("iMCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [SEL ("",
                 [VRF ("PWDATA",
                   (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                  CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                SEL ("",
                 [VRF ("iMCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                   []);
                  CNST (32, SHEX 3); CNST (32, SHEX 32)])])])])])]);
      ("",
       [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Land,
         [LOGIC (Lor,
           [LOGIC (Land,
             [UNRY (Unot,
               [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
              VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), [])]);
            LOGIC (Land,
             [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
              VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []), [])])]);
          VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iPERE", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iFERE", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iBIRE", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), []);
          CMP (Ceq,
           [CNST (32, SHEX 3);
            SEL ("",
             [VRF ("iRXFIFOQ",
               (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), []);
              CNST (32, SHEX 4); CNST (32, SHEX 32)])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [CMP (Cneq,
         [CNST (32, SHEX 7);
          VRF ("iFECounter", (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []),
           [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Lor,
             [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), []);
              CMP (Ceq,
               [CNST (32, SHEX 3);
                SEL ("",
                 [VRF ("iRXFIFOQ",
                   (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), []);
                  CNST (32, SHEX 4); CNST (32, SHEX 32)])])]);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [LOGIC (Land,
         [UNRY (Unot,
           [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), [])]);
          VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [ARITH (Asub,
             [VRF ("iFECounter",
               (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), []);
              CNST (32, SHEX 7)]);
            VRF ("iFECounter", (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []),
             [])])])]);
      ("",
       [LOGIC (Land,
         [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), []);
          UNRY (Unot,
           [VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [ASGN (true, "",
           [ARITH (Aadd,
             [CNST (32, SHEX 7);
              VRF ("iFECounter",
               (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])]);
            VRF ("iFECounter", (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []),
             [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Land,
             [UNRY (Unot,
               [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), [])]);
              VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []), [])]);
            BGN (None,
             [ASGN (true, "",
               [ARITH (Asub,
                 [VRF ("iFECounter",
                   (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), []);
                  CNST (32, SHEX 7)]);
                VRF ("iFECounter",
                 (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])])])])])]);
      ("",
       [VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 7);
            VRF ("iFECounter", (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []),
             [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Land,
             [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), []);
              UNRY (Unot,
               [VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []), [])])]);
            BGN (None,
             [ASGN (true, "",
               [ARITH (Aadd,
                 [CNST (32, SHEX 7);
                  VRF ("iFECounter",
                   (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])]);
                VRF ("iFECounter",
                 (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])])]);
            BGN (None,
             [IF ("",
               [LOGIC (Land,
                 [UNRY (Unot,
                   [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), [])]);
                  VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []), [])]);
                BGN (None,
                 [ASGN (true, "",
                   [ARITH (Asub,
                     [VRF ("iFECounter",
                       (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), 
                       []);
                      CNST (32, SHEX 7)]);
                    VRF ("iFECounter",
                     (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])])])])])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 7);
              VRF ("iFECounter",
               (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
        BGN (None,
         [BGN (None,
           [IF ("",
             [LOGIC (Land,
               [LOGIC (Lor,
                 [LOGIC (Land,
                   [UNRY (Unot,
                     [VRF ("iFCR_FIFOEnable",
                       (BASDTYP, "logic", TYPNONE, []), [])]);
                    VRF ("iLSR_DR", (BASDTYP, "logic", TYPNONE, []), [])]);
                  LOGIC (Land,
                   [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                     []);
                    VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []), [])])]);
                VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_OE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iPERE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_PE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iFERE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_FE", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iBIRE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iLSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_BI", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [CMP (Cneq,
               [CNST (32, SHEX 7);
                VRF ("iFECounter",
                 (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Lor,
                   [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), []);
                    CMP (Ceq,
                     [CNST (32, SHEX 3);
                      SEL ("",
                       [VRF ("iRXFIFOQ",
                         (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), 
                         []);
                        CNST (32, SHEX 4); CNST (32, SHEX 32)])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iLSR_FIFOERR", (BASDTYP, "logic", TYPNONE, []),
                       [])])])])])]);
            IF ("",
             [VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 7);
                  VRF ("iFECounter",
                   (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), [])])]);
              BGN (None,
               [IF ("",
                 [LOGIC (Land,
                   [VRF ("iFEIncrement", (BASDTYP, "logic", TYPNONE, []), []);
                    UNRY (Unot,
                     [VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []),
                       [])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [ARITH (Aadd,
                       [CNST (32, SHEX 7);
                        VRF ("iFECounter",
                         (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), 
                         [])]);
                      VRF ("iFECounter",
                       (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), 
                       [])])]);
                  BGN (None,
                   [IF ("",
                     [LOGIC (Land,
                       [UNRY (Unot,
                         [VRF ("iFEIncrement",
                           (BASDTYP, "logic", TYPNONE, []), [])]);
                        VRF ("iFEDecrement", (BASDTYP, "logic", TYPNONE, []),
                         [])]);
                      BGN (None,
                       [ASGN (true, "",
                         [ARITH (Asub,
                           [VRF ("iFECounter",
                             (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []),
                             []);
                            CNST (32, SHEX 7)]);
                          VRF ("iFECounter",
                           (BASDTYP, "logic", TYPRNG (HEX 6, HEX 0), []), 
                           [])])])])])])])])])])]);
      ("",
       [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iCTSnRE", (BASDTYP, "logic", TYPNONE, []), []);
          VRF ("iCTSnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iDSRnRE", (BASDTYP, "logic", TYPNONE, []), []);
          VRF ("iDSRnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iRInFE", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iDCDnRE", (BASDTYP, "logic", TYPNONE, []), []);
          VRF ("iDCDnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])])]);
        BGN (None,
         [BGN (None,
           [IF ("",
             [LOGIC (Lor,
               [VRF ("iCTSnRE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iCTSnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_dCTS", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iDSRnRE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iDSRnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_dDSR", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [VRF ("iRInFE", (BASDTYP, "logic", TYPNONE, []), []);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_TERI", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iDCDnRE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iDCDnFE", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [IF ("",
                 [VRF ("iMSRRead", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("iMSR_dDCD", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])])]);
      ("",
       [VRF ("iSCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [SEL ("",
             [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)]);
            VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 8);
            VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])]);
        BGN (None,
         [IF ("",
           [VRF ("iSCRWrite", (BASDTYP, "logic", TYPNONE, []), []);
            BGN (None,
             [ASGN (true, "",
               [SEL ("",
                 [VRF ("PWDATA",
                   (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []), []);
                  CNST (32, SHEX 5); CNST (32, SHEX 32)]);
                VRF ("iSCR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                 [])])])])])]);
      ("",
       [VRF ("iTXEnable", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 2);
              VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
               [])])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 2);
            VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
             [])])])]);
      ("",
       [VRF ("iTXFinished", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 2);
            VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
             [])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 2);
            VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
             [])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 2);
              VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
               [])]);
            ASGN (true, "",
             [CNST (32, SHEX 8);
              VRF ("iTSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])])])]);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
            CS ("",
             [VRF ("tx_State", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
               []);
              CSITM ("",
               [CNST (32, SHEX 2);
                IF ("",
                 [VRF ("iTXEnable", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 1);
                        VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
                      ASGN (true, "",
                       [CNST (32, SHEX 2);
                        VRF ("tx_State",
                         (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                         [])])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 2);
                      VRF ("tx_State",
                       (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                       [])])])])]);
              CSITM ("",
               [CNST (32, SHEX 2);
                BGN (None,
                 [ASGN (true, "",
                   [VRF ("iTXFIFOQ",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
                    VRF ("iTSR",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 2);
                    VRF ("tx_State",
                     (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])])]);
              CSITM ("",
               [CNST (32, SHEX 2);
                BGN (None,
                 [IF ("",
                   [VRF ("iTXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 2);
                        VRF ("tx_State",
                         (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                         [])])]);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 2);
                        VRF ("tx_State",
                         (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), 
                         [])])])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXRunning", (BASDTYP, "logic", TYPNONE, []), [])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])])])]);
              CSITM ("",
               [CNST (32, SHEX 2);
                ASGN (true, "",
                 [CNST (32, SHEX 2);
                  VRF ("tx_State",
                   (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])]);
              CSITM ("",
               [ASGN (true, "",
                 [CNST (32, SHEX 2);
                  VRF ("tx_State",
                   (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []), [])])])])])])]);
      ("",
       [UNRY (Unot,
         [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CAT ("",
               [VRF ("iRXBI", (BASDTYP, "logic", TYPNONE, []), []);
                CAT ("",
                 [VRF ("iRXFE", (BASDTYP, "logic", TYPNONE, []), []);
                  CAT ("",
                   [VRF ("iRXPE", (BASDTYP, "logic", TYPNONE, []), []);
                    VRF ("iRXData",
                     (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])])])]);
              VRF ("iRXFIFOD",
               (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), [])]);
            IF ("",
             [UNRY (Unot,
               [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [UNRY (Unot,
         [VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [IF ("",
           [UNRY (Unot,
             [VRF ("iRXFIFOFull", (BASDTYP, "logic", TYPNONE, []), [])]);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 11);
              VRF ("iRXFIFOD",
               (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), [])])])]);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [VRF ("iFCR_RXFIFOReset", (BASDTYP, "logic", TYPNONE, []), []);
              VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
            CS ("",
             [VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), []);
              CSITM ("",
               [CNST (32, SHEX 1);
                IF ("",
                 [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), []);
                  BGN (None,
                   [BGN (None,
                     [ASGN (true, "",
                       [CAT ("",
                         [VRF ("iRXBI", (BASDTYP, "logic", TYPNONE, []), []);
                          CAT ("",
                           [VRF ("iRXFE", (BASDTYP, "logic", TYPNONE, []),
                             []);
                            CAT ("",
                             [VRF ("iRXPE", (BASDTYP, "logic", TYPNONE, []),
                               []);
                              VRF ("iRXData",
                               (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
                               [])])])]);
                        VRF ("iRXFIFOD",
                         (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []), 
                         [])]);
                      IF ("",
                       [UNRY (Unot,
                         [VRF ("iFCR_FIFOEnable",
                           (BASDTYP, "logic", TYPNONE, []), [])]);
                        BGN (None,
                         [ASGN (true, "",
                           [CNST (32, SHEX 1);
                            VRF ("iRXFIFOClear",
                             (BASDTYP, "logic", TYPNONE, []), [])])])]);
                      ASGN (true, "",
                       [CNST (32, SHEX 1);
                        VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
                  BGN (None,
                   [ASGN (true, "",
                     [CNST (32, SHEX 1);
                      VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])]);
              CSITM ("",
               [CNST (32, SHEX 1);
                BGN (None,
                 [IF ("",
                   [VRF ("iFCR_FIFOEnable", (BASDTYP, "logic", TYPNONE, []),
                     []);
                    BGN (None,
                     [IF ("",
                       [UNRY (Unot,
                         [VRF ("iRXFIFOFull",
                           (BASDTYP, "logic", TYPNONE, []), [])]);
                        BGN (None,
                         [ASGN (true, "",
                           [CNST (32, SHEX 1);
                            VRF ("iRXFIFOWrite",
                             (BASDTYP, "logic", TYPNONE, []), [])])])])]);
                    BGN (None,
                     [ASGN (true, "",
                       [CNST (32, SHEX 1);
                        VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []),
                         [])])])]);
                  ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])]);
              CSITM ("",
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("rx_State", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
      ("",
       [LOGIC (Land,
         [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
          LOGIC (Lor,
           [UNRY (Unot,
             [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), [])]);
            LOGIC (Land,
             [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
              VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [UNRY (Unot,
           [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), [])]);
          LOGIC (Land,
           [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
            VRF ("iRXFIFOTrigger", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Land,
             [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
              LOGIC (Lor,
               [UNRY (Unot,
                 [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), [])]);
                LOGIC (Land,
                 [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
                  VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [IF ("",
           [LOGIC (Lor,
             [UNRY (Unot,
               [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), [])]);
              LOGIC (Land,
               [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iRXFIFOTrigger", (BASDTYP, "logic", TYPNONE, []), [])])]);
            BGN (None,
             [ASGN (true, "",
               [CNST (32, SHEX 1);
                VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
            BGN (None,
             [IF ("",
               [LOGIC (Land,
                 [VRF ("iMCR_RTS", (BASDTYP, "logic", TYPNONE, []), []);
                  LOGIC (Lor,
                   [UNRY (Unot,
                     [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), [])]);
                    LOGIC (Land,
                     [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), []);
                      VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []),
                       [])])])]);
                BGN (None,
                 [ASGN (true, "",
                   [CNST (32, SHEX 1);
                    VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])])])])])])]);
      ("",
       [UNRY (Unot,
         [VRF ("iBaudtick16x", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
          UNRY (Unot,
           [VRF ("iMCR_OUT1", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
          UNRY (Unot,
           [VRF ("iMCR_OUT2", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
          UNRY (Unot, [VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
          UNRY (Unot,
           [VRF ("iMCR_DTR", (BASDTYP, "logic", TYPNONE, []), [])])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [LOGIC (Lor,
         [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
          VRF ("iSOUT", (BASDTYP, "logic", TYPNONE, []), [])]);
        BGN (None,
         [ASGN (true, "",
           [CNST (32, SHEX 1);
            VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])])]);
      ("",
       [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])])]);
        BGN (None,
         [BGN (None,
           [ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])]);
            ASGN (true, "",
             [CNST (32, SHEX 1);
              VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])]);
            IF ("",
             [UNRY (Unot,
               [VRF ("iBaudtick16x", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iMCR_OUT1", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("OUT1N", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iMCR_OUT2", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("OUT2N", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iRTS", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("RTSN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                UNRY (Unot,
                 [VRF ("iMCR_DTR", (BASDTYP, "logic", TYPNONE, []), [])])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("DTRN", (BASDTYP, "logic", TYPNONE, []), [])])])]);
            IF ("",
             [LOGIC (Lor,
               [VRF ("iMCR_LOOP", (BASDTYP, "logic", TYPNONE, []), []);
                VRF ("iSOUT", (BASDTYP, "logic", TYPNONE, []), [])]);
              BGN (None,
               [ASGN (true, "",
                 [CNST (32, SHEX 1);
                  VRF ("SOUT", (BASDTYP, "logic", TYPNONE, []), [])])])])])])]);
      ("",
       [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [VRF ("iDLL", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
            SEL ("",
             [VRF ("PRDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
        BGN (None,
         [ASGN (true, "",
           [VRF ("iRBR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
            SEL ("",
             [VRF ("PRDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)])])])]);
      ("",
       [VRF ("iLCR_DLAB", (BASDTYP, "logic", TYPNONE, []), []);
        BGN (None,
         [ASGN (true, "",
           [VRF ("iDLM", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
            SEL ("",
             [VRF ("PRDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)])])]);
        BGN (None,
         [ASGN (true, "",
           [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
            SEL ("",
             [VRF ("PRDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
               []);
              CNST (32, SHEX 5); CNST (32, SHEX 32)])])])])]};
  imp = {contents = []};
  inst =
   {contents =
     [("UART_IS_SIN",
       ("", "slib_input_sync",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("SIN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Doutput,
          [VRF ("iSINr", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IS_CTS",
       ("", "slib_input_sync",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("CTSN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Doutput,
          [VRF ("iCTSNs", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IS_DSR",
       ("", "slib_input_sync",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("DSRN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Doutput,
          [VRF ("iDSRNs", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IS_DCD",
       ("", "slib_input_sync",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("DCDN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Doutput,
          [VRF ("iDCDNs", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IS_RI",
       ("", "slib_input_sync",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("RIN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Doutput,
          [VRF ("iRINs", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IF_CTS",
       ("", "slib_input_filter",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Dinput,
          [VRF ("iCTSNs", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber5", Doutput,
          [VRF ("iCTSn", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IF_DSR",
       ("", "slib_input_filter",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Dinput,
          [VRF ("iDSRNs", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber5", Doutput,
          [VRF ("iDSRn", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IF_DCD",
       ("", "slib_input_filter",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Dinput,
          [VRF ("iDCDNs", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber5", Doutput,
          [VRF ("iDCDn", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IF_RI",
       ("", "slib_input_filter",
        [PORT ("", "__pinNumber1", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber2", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber3", Dinput,
          [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber4", Dinput,
          [VRF ("iRINs", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "__pinNumber5", Doutput,
          [VRF ("iRIn", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IIC",
       ("", "uart_interrupt",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "IER", Dinput,
          [SEL ("",
            [VRF ("iIER", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
             CNST (32, SHEX 3); CNST (32, SHEX 32)])]);
         PORT ("", "LSR", Dinput,
          [SEL ("",
            [VRF ("iLSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
             CNST (32, SHEX 3); CNST (32, SHEX 32)])]);
         PORT ("", "THI", Dinput,
          [VRF ("iTHRInterrupt", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RDA", Dinput,
          [VRF ("iRDAInterrupt", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "CTI", Dinput,
          [VRF ("iCharTimeout", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "AFE", Dinput,
          [VRF ("iMCR_AFE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "MSR", Dinput,
          [SEL ("",
            [VRF ("iMSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
             CNST (32, SHEX 3); CNST (32, SHEX 32)])]);
         PORT ("", "IIR", Doutput,
          [SEL ("",
            [VRF ("iIIR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), []);
             CNST (32, SHEX 3); CNST (32, SHEX 32)])]);
         PORT ("", "INT", Doutput,
          [VRF ("INT", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_IIC_THRE_ED",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iLSR_THRERE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput, [])]));
      ("UART_PEDET",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iRXFIFOPE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iPERE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput, [])]));
      ("UART_FEDET",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iRXFIFOFE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iFERE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput, [])]));
      ("UART_BIDET",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iRXFIFOBI", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iBIRE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput, [])]));
      ("UART_ED_CTS",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iMSR_CTS", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iCTSnRE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput,
          [VRF ("iCTSnFE", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_ED_DSR",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iMSR_DSR", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iDSRnRE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput,
          [VRF ("iDSRnFE", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_ED_RI",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iMSR_RI", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iRInRE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput,
          [VRF ("iRInFE", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_ED_DCD",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iMSR_DCD", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iDCDnRE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput,
          [VRF ("iDCDnFE", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_BG16",
       ("", "uart_baudgen",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "CE", Dinput, [CNST (32, SHEX 1)]);
         PORT ("", "CLEAR", Dinput, [CNST (32, SHEX 1)]);
         PORT ("", "DIVIDER", Dinput,
          [VRF ("iBaudgenDiv",
            (BASDTYP, "logic", TYPRNG (HEX 15, HEX 0), []), [])]);
         PORT ("", "BAUDTICK", Doutput,
          [VRF ("iBaudtick16x", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_BG2",
       ("", "slib_clock_div",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "CE", Dinput,
          [VRF ("iBaudtick16x", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "Q", Doutput,
          [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_RCLK",
       ("", "slib_edge_detect",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iBAUDOUTN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RE", Doutput,
          [VRF ("iRCLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput, [])]));
      ("UART_TXFF",
       ("", "slib_fifo",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "CLEAR", Dinput,
          [VRF ("iFCR_TXFIFOReset", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "WRITE", Dinput,
          [VRF ("iTXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "READ", Dinput,
          [VRF ("iTXFIFORead", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [SEL ("",
            [VRF ("PWDATA", (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), []),
              []);
             CNST (32, SHEX 5); CNST (32, SHEX 32)])]);
         PORT ("", "Q", Doutput,
          [VRF ("iTXFIFOQ", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []),
            [])]);
         PORT ("", "EMPTY", Doutput,
          [VRF ("iTXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FULL", Doutput,
          [VRF ("iTXFIFO64Full", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "USAGE", Doutput,
          [VRF ("iTXFIFOUsage",
            (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])])]));
      ("UART_RXFF",
       ("", "slib_fifo",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "CLEAR", Dinput,
          [VRF ("iRXFIFOClear", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "WRITE", Dinput,
          [VRF ("iRXFIFOWrite", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "READ", Dinput,
          [VRF ("iRBRRead", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "D", Dinput,
          [VRF ("iRXFIFOD", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []),
            [])]);
         PORT ("", "Q", Doutput,
          [VRF ("iRXFIFOQ", (BASDTYP, "logic", TYPRNG (HEX 10, HEX 0), []),
            [])]);
         PORT ("", "EMPTY", Doutput,
          [VRF ("iRXFIFOEmpty", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FULL", Doutput,
          [VRF ("iRXFIFO64Full", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "USAGE", Doutput,
          [VRF ("iRXFIFOUsage",
            (BASDTYP, "logic", TYPRNG (HEX 5, HEX 0), []), [])])]));
      ("UART_TX",
       ("", "uart_transmitter",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "TXCLK", Dinput,
          [VRF ("iBaudtick2x", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "TXSTART", Dinput,
          [VRF ("iTXStart", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "CLEAR", Dinput, [CNST (32, SHEX 1)]);
         PORT ("", "WLS", Dinput,
          [VRF ("iLCR_WLS", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
            [])]);
         PORT ("", "STB", Dinput,
          [VRF ("iLCR_STB", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "PEN", Dinput,
          [VRF ("iLCR_PEN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "EPS", Dinput,
          [VRF ("iLCR_EPS", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "SP", Dinput,
          [VRF ("iLCR_SP", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "BC", Dinput,
          [VRF ("iLCR_BC", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "DIN", Dinput,
          [VRF ("iTSR", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
         PORT ("", "TXFINISHED", Doutput,
          [VRF ("iTXFinished", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "SOUT", Doutput,
          [VRF ("iSOUT", (BASDTYP, "logic", TYPNONE, []), [])])]));
      ("UART_RX",
       ("", "uart_receiver",
        [PORT ("", "CLK", Dinput,
          [VRF ("CLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RST", Dinput,
          [VRF ("iRST", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RXCLK", Dinput,
          [VRF ("iRCLK", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "RXCLEAR", Dinput, [CNST (32, SHEX 1)]);
         PORT ("", "WLS", Dinput,
          [VRF ("iLCR_WLS", (BASDTYP, "logic", TYPRNG (HEX 1, HEX 0), []),
            [])]);
         PORT ("", "STB", Dinput,
          [VRF ("iLCR_STB", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "PEN", Dinput,
          [VRF ("iLCR_PEN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "EPS", Dinput,
          [VRF ("iLCR_EPS", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "SP", Dinput,
          [VRF ("iLCR_SP", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "SIN", Dinput,
          [VRF ("iSIN", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "PE", Doutput,
          [VRF ("iRXPE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "FE", Doutput,
          [VRF ("iRXFE", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "BI", Doutput,
          [VRF ("iRXBI", (BASDTYP, "logic", TYPNONE, []), [])]);
         PORT ("", "DOUT", Doutput,
          [VRF ("iRXData", (BASDTYP, "logic", TYPRNG (HEX 7, HEX 0), []), [])]);
         PORT ("", "RXFINISHED", Doutput,
          [VRF ("iRXFinished", (BASDTYP, "logic", TYPNONE, []), [])])]))]};
  cnst =
   {contents =
     [("iTXClear", (false, (32, SHEX 1)));
      ("iRXClear", (false, (32, SHEX 1))); ("PREADY", (false, (32, SHEX 1)));
      ("PSLVERR", (false, (32, SHEX 1)))]};
  needed = {contents = []}; remove_interfaces = false;
  names'' =
   [("SOUT", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("SIN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("RIN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("DCDN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("DSRN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("CTSN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("DTRN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("RTSN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("OUT2N", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("OUT1N", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("INT", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("PSLVERR", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("PREADY", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("PRDATA", {contents = (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), [])});
    ("PWDATA", {contents = (BASDTYP, "logic", TYPRNG (HEX 31, HEX 0), [])});
    ("PADDR", {contents = (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), [])});
    ("PWRITE", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("PENABLE", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("PSEL", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("RSTN", {contents = (BASDTYP, "logic", TYPNONE, [])});
    ("CLK", {contents = (BASDTYP, "logic", TYPNONE, [])})]}))
