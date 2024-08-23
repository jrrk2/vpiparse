
module casetest(
	input wire		CLK,
	input wire		RST,
	input wire		RXCLK,
	input wire		RXCLEAR,
	input wire	[1:0] 	WLS,
	input wire		STB,
	input wire		PEN,
	input wire		EPS,
	input wire		SP,
	input wire		SIN,
	output logic		PE,
	output logic		FE,
	output logic		BI,
	output logic	[7:0] 	DOUT,
	output logic		RXFINISHED);  
  
typedef enum logic [2:0] {IDLE,
START,
DATA,
PAR,
STOP,
MWAIT} state_type;  

state_type CState, NState;  

reg [3:0] iBaudCount;  
reg iBaudCountClear;  
reg iBaudStep;  
reg iBaudStepD;  
reg iFilterClear;  
reg iFSIN;  
reg iFStopBit;  
reg iParity;  
reg iParityReceived;  
reg [3:0] iDataCount;  
reg iDataCountInit;  
reg iDataCountFinish;  
reg iRXFinished;  
reg iFE;  
reg iBI;  
reg iNoStopReceived;  
reg [7:0] iDOUT;  

always @(CState or SIN or iFSIN or iFStopBit or iBaudStep or iBaudCount or iDataCountFinish or PEN or WLS or STB)
begin
NState = IDLE;  
iBaudCountClear =  1'b0;  
iDataCountInit =  1'b0;  
iRXFinished =  1'b0;  
case (CState)
  IDLE:
    begin
  if ((SIN ==  1'b0))
          begin
      NState = START;  
              end
      
iBaudCountClear =  1'b1;  
    iDataCountInit =  1'b1;  
      end
  
  START:
    begin
  iDataCountInit =  1'b1;  
    if ((iBaudStep ==  1'b1))
          begin
      if ((iFSIN ==  1'b0))
                  begin
          NState = DATA;  
                      end
          
      end
       else
      begin
      NState = START;  
              end
        end
  
  DATA:
    begin
  if ((iDataCountFinish ==  1'b1))
          begin
      if ((PEN ==  1'b1))
                  begin
          NState = PAR;  
                      end
           else
          begin
          NState = STOP;  
                      end
                end
       else
      begin
      NState = DATA;  
              end
        end
  
  PAR:
    begin
  if ((iBaudStep ==  1'b1))
          begin
      NState = STOP;  
              end
       else
      begin
      NState = PAR;  
              end
        end
  
  STOP:
    begin
  if ((iBaudCount[3] ==  1'b1))
          begin
      if ((iFStopBit ==  1'b0))
                  begin
          iRXFinished =  1'b1;  
            NState = MWAIT;  
                      end
           else
          begin
          iRXFinished =  1'b1;  
            NState = IDLE;  
                      end
                end
       else
      begin
      NState = STOP;  
              end
        end
  
  MWAIT:
    begin
  if ((SIN ==  1'b0))
          begin
      NState = MWAIT;  
              end
      
  end
  
  default:
    begin
  begin end  end
  
endcase

end

endmodule
