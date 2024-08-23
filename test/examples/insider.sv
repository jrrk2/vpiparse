package riscv;
    typedef enum logic [3:0] {
       ModeOff  = 0,
       ModeSv32 = 1,
       ModeSv39 = 8,
       ModeSv48 = 9,
       ModeSv57 = 10,
       ModeSv64 = 11
    } vm_mode_t;
    localparam XLEN = 64;
    localparam VLEN       = (XLEN == 32) ? 32 : 64;     
    localparam PLEN       = (XLEN == 32) ? 34 : 56;     
    localparam IS_XLEN32  = (XLEN == 32) ? 1'b1 : 1'b0;
    localparam IS_XLEN64  = (XLEN == 32) ? 1'b0 : 1'b1;
    localparam ModeW      = (XLEN == 32) ? 1 : 4;
    localparam ASIDW      = (XLEN == 32) ? 9 : 16;
    localparam PPNW       = (XLEN == 32) ? 22 : 44;
    localparam vm_mode_t MODE_SV = (XLEN == 32) ? ModeSv32 : ModeSv39;
    localparam SV         = (MODE_SV == ModeSv32) ? 32 : 39;
    localparam VPN2       = (VLEN-31 < 8) ? VLEN-31 : 8;
    localparam  FPU_EN     = 1'b1;  
    typedef logic [XLEN-1:0] xlen_t;
endpackage; // riscv
   
module insider(input [6:0] op, output logic rslt);

    typedef enum logic[3:0] {
        NONE,       
        LOAD,       
        STORE,      
        ALU,        
        CTRL_FLOW,  
        MULT,       
        CSR,        
        FPU,        
        FPU_VEC     
    } fu_t;
   
    localparam NR_SB_ENTRIES = 8;  
    localparam TRANS_ID_BITS = $clog2(NR_SB_ENTRIES);  
    localparam bit RVF = (riscv::IS_XLEN64 | riscv::IS_XLEN32) & riscv::FPU_EN;  
    localparam bit RVD = (riscv::IS_XLEN64 ? 1:0) & riscv::FPU_EN;               
    localparam bit RVA = 1'b1;  
    localparam bit XF16    = 1'b0;  
    localparam bit XF16ALT = 1'b0;  
    localparam bit XF8     = 1'b0;  
    localparam bit XFVEC   = 1'b0;  
    localparam bit FP_PRESENT = RVF | RVD | XF16 | XF16ALT | XF8;
 
   typedef enum logic [6:0] {  
                               ADD, SUB, ADDW, SUBW,
                               XORL, ORL, ANDL,
                               SRA, SRL, SLL, SRLW, SLLW, SRAW,
                               LTS, LTU, GES, GEU, EQ, NE,
                               JALR, BRANCH,
                               SLTS, SLTU,
                               MRET, SRET, DRET, ECALL, WFI, FENCE, FENCE_I, SFENCE_VMA, CSR_WRITE, CSR_READ, CSR_SET, CSR_CLEAR,
                               LD, SD, LW, LWU, SW, LH, LHU, SH, LB, SB, LBU,
                               AMO_LRW, AMO_LRD, AMO_SCW, AMO_SCD,
                               AMO_SWAPW, AMO_ADDW, AMO_ANDW, AMO_ORW, AMO_XORW, AMO_MAXW, AMO_MAXWU, AMO_MINW, AMO_MINWU,
                               AMO_SWAPD, AMO_ADDD, AMO_ANDD, AMO_ORD, AMO_XORD, AMO_MAXD, AMO_MAXDU, AMO_MIND, AMO_MINDU,
                               MUL, MULH, MULHU, MULHSU, MULW,
                               DIV, DIVU, DIVW, DIVUW, REM, REMU, REMW, REMUW,
                               FLD, FLW, FLH, FLB, FSD, FSW, FSH, FSB,
                               FADD, FSUB, FMUL, FDIV, FMIN_MAX, FSQRT, FMADD, FMSUB, FNMSUB, FNMADD,
                               FCVT_F2I, FCVT_I2F, FCVT_F2F, FSGNJ, FMV_F2X, FMV_X2F,
                               FCMP,
                               FCLASS,
                               VFMIN, VFMAX, VFSGNJ, VFSGNJN, VFSGNJX, VFEQ, VFNE, VFLT, VFGE, VFLE, VFGT, VFCPKAB_S, VFCPKCD_S, VFCPKAB_D, VFCPKCD_D
                             } fu_op;
    typedef struct packed {
        fu_t                      fu;
        fu_op                     operator;
        riscv::xlen_t             operand_a;
        riscv::xlen_t             operand_b;
        riscv::xlen_t             imm;
        logic [TRANS_ID_BITS-1:0] trans_id;
    } fu_data_t;

    function automatic logic op_is_branch (input fu_op op);
        unique case (op) inside
            EQ, NE, LTS, GES, LTU, GEU: return 1'b1;
            default                   : return 1'b0;  
        endcase
    endfunction

   always @(op)
     rslt = op_is_branch(op);
   
endmodule
   
