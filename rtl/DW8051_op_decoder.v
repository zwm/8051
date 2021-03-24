// $Id: DW8051_op_decoder.v,v 1.1 1996/07/25 17:43:07 gina Exp $
//------------------------------------------------------------------------------
//
//        This confidential and proprietary software may be used only
//     as authorized by a licensing agreement from Synopsys Inc.
//     In the event of publication, the following notice is applicable:
//
//                    (C) COPYRIGHT 1996   SYNOPSYS INC.
//                          ALL RIGHTS RESERVED
//
//        The entire notice above must be reproduced on all authorized
//        copies.
//
// FILE: DW8051_op_decoder.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 instruction decoder (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        28.05.96        Verilog version created
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//		        May 20,1998	Removed Blocking assignments for
//					oph and opl. Changed all references to
//					oph/opl to the actual op[a:b]
//					dereferences.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_op_decoder (op,
                          int,
                          idle_mode_n,
                          itype,
                          last_cycle,
                          src,
                          src_cycle,
                          dest,
                          alu_op,
                          chg_flags,
                          rmw
			  );
 input [7:0] op;
 input int;
 input idle_mode_n;
 output [4:0] itype;		// 0..31
 output [2:0] last_cycle;	// 0..7
 output [3:0] src;		// 0..15
 output [1:0] src_cycle;	// 0..3
 output [3:0] dest;		// 0..15
 output [5:0] alu_op;
 output chg_flags;
 output rmw;

//------------------------------------------------------------------------------
//                             DESCRIPTION
//------------------------------------------------------------------------------
//
//      src                             dest
//      1       accumulator             1       accumulator
//      2       register Rn             2       register Rn
//      3       direct data (2nd cycle) 3       direct data (type 1)
//      4       indirect @Ri (1Byte)    4       indirect @Ri (1Byte)
//      5       #data                   5       accu direct
//      6       ext RAM, @Ri            6       ext.RAM, @Ri
//      7       bit                     7       bit
//      8                               8       direct (type 2)
//      9                               9       direct (type 3)
//      10                              10      indirect (type 2)
//      11                              11      indirect (type 3)
//      12      @SP                     12      @SP
//      13                              13      dptr (src instr)
//      14      ROM                     14      dptr (src add16_sum)
//      15      ext.RAM, @DPTR          15      ext.RAM, @DPTR
//
//
// instruction type
//
// One Machine-Cycle instructions:
// 0    none (default after reset)
// 0    NOP
// 1    RR A, INC A, INC @Ri, INC Rn, RRC A, DEC A, DEC @Ri, DEC Rn,
//      RL A, RLC A, CPL C, CLR C, SWAP A, SETB C, DA A, CLR A,
//      MOV A,@Ri, MOV A,Rn, CPL A, MOV @Ri,A, MOV Rn,A
// 1    XCH A,@Ri, XCH A,Rn, XCHD A,@Ri
// 2    idle_mode
// 3    ADD A,@Ri, ADD A,Rn, ADDC A,@Ri, ADDC A,Rn, ORL A,@Ri, ORL A,Rn,
//      ANL A,@Ri, ANL A,Rn, XRL A,@Ri, XRL A,Rn, SUBB A,@Ri, SUBB A,Rn
//
// Two Machine-Cycle instructions:
// 4    ADD A,#data, ADD A,direct, ADDC A,#data, ADDC A,direct,
//      ORL direct,A, ORL A,#data, ORL A,direct, ANL direct,A,
//      ANL A,#data, ANL A,direct, XRL direct,A, XRL A,#data,
//      XRL A,direct, SUBB A,#data, SUBB A,direct
// 5    INC direct, DEC direct, MOV A,#data, MOV @Ri,#data,
//      MOV Rn,#data, MOV direct,@Ri, MOV direct,Rn, MOV bit,C,
//      MOV @Ri,direct, MOV Rn,direct, CPL bit, CLR bit, SETB bit,
//      MOV A,direct, MOV direct,A
// 5    XCH A,direct
// 6    ORL C,bit, ANL C,bit, ORL C,/bit, MOV C,bit, ANL C,/bit
// 7    PUSH, POP
// 8    MOVX A,@DPTR, MOVX A,@Ri, MOVX @DPTR,A, MOVX @Ri,A
//
// Three Machine-Cycle instructions:
// 9    MOV direct,direct
// 10   MOV direct,#data
// 11   ORL direct,#data, ANL direct,#data, XRL direct,#data
// 12   INC DPTR
// 13   DJNZ Rn,rel
// 14   ACALL
// 15   AJMP
// 16   SJMP
// 17   JMP @A+DPTR
// 18   JC,JNC,JZ,JNZ
// 19   MOVC A,@A+PC, MOVC A,@A+DPTR
// 20   MOV DPTR,#data
//
// Four Machine-Cycle instructions:
// 21   JBC, JB, JNB
// 22   CJNE A,#data,rel, CJNE A,direct,rel CJNE @Ri,#data,rel,
//      CJNE Rn,#data,rel
// 23   LCALL
// 24   LJMP
// 25   RET
// 25   RETI
// 26   DJNZ direct,rel
// 27   interrupts
//
// Five Machine-Cycle instructions:
// 28   MUL AB
// 29   DIV AB
//
//
// itypes NOT used: 30,31
//
// opcode      type     src     dest    rmw     Bytes   mnem.   operands
//                                              Code/Ex
// 0000_0000    0       -       -       -       1/1     NOP     -
// 0000_0001    15      -       -       -       2/3     AJMP    code addr
// 0000_0010    24      -       -       -       3/4     LJMP    code addr
// 0000_0011    1       1       1       -       1/1     RR      A
// 0000_0100    1       1       1       -       1/1     INC     A
// 0000_0101    5       3       3       1       2/2     INC     direct
// 0000_011x    1       4       4       -       1/1     INC     @Ri
// 0000_1xxx    1       2       2       -       1/1     INC     Rn
//
// 0001_0000    21      7       7       1       3/4     JBC   bit addr,code addr
// 0001_0001    14      -       -       -       2/3     ACALL   code addr
// 0001_0010    23      -       -       -       3/4     LCALL   code addr
// 0001_0011    1       1       1       -       1/1     RRC     A
// 0001_0100    1       1       1       -       1/1     DEC     A
// 0001_0101    5       3       3       1       2/2     DEC     direct
// 0001_011x    1       4       4       -       1/1     DEC     @Ri
// 0001_1xxx    1       2       2       -       1/1     DEC     Rn
//
// 0010_0000    21      7       -       -       3/4     JB    bit addr,code addr
// 0010_0001    15      -       -       -       2/3     AJMP    code addr
// 0010_0010    25      -       -       -       1/4     RET
// 0010_0011    1       1       1       -       1/1     RL      A
// 0010_0100    4       5       1       -       2/2     ADD     A,#data
// 0010_0101    4       3       1       -       2/2     ADD     A,direct
// 0010_011x    3       4       1       -       1/1     ADD     A,@Ri
// 0010_1xxx    3       2       1       -       1/1     ADD     A,Rn
//
// 0011_0000    21      7       -       -       3/4     JNB     bit,addr,code
// 0011_0001    14      -       -       -       2/3     ACALL   addr
// 0011_0010    25      -       -       -       1/4     RETI    (code addr)
// 0011_0011    1       1       1       -       1/1     RLC     A
// 0011_0100    4       5       1       -       2/2     ADDC    A,#data
// 0011_0101    4       3       1       -       2/2     ADDC    A,direct
// 0011_011x    3       4       1       -       1/1     ADDC    A,@Ri
// 0011_1xxx    3       2       1       -       1/1     ADDC    A,Rn
//
// 0100_0000    18      -       -       -       2/3     JC      code addr
// 0100_0001    15      -       -       -       2/3     AJMP    code addr
// 0100_0010    4       3       3       1       2/2     ORL     direct,A
// 0100_0011    11      3       8       1       3/3     ORL     direct,#data
// 0100_0100    4       5       1       -       2/2     ORL     A,#data
// 0100_0101    4       3       1       -       2/2     ORL     A,direct
// 0100_011x    3       4       1       -       1/1     ORL     A,@Ri
// 0100_1xxx    3       2       1       -       1/1     ORL     A,Rn
//
// 0101_0000    18      -       -       -       2/3     JNC     code addr
// 0101_0001    14      -       -       -       2/3     ACALL   code addr
// 0101_0010    4       3       3       1       2/2     ANL     direct,A
// 0101_0011    11      3       8       1       3/3     ANL     direct,#data
// 0101_0100    4       5       1       -       2/2     ANL     A,#data
// 0101_0101    4       3       1       -       2/2     ANL     A,direct
// 0101_011x    3       4       1       -       1/1     ANL     A,@Ri
// 0101_1xxx    3       2       1       -       1/1     ANL     A,Rn
//
// 0110_0000    18      1       -       -       2/3     JZ      code addr
// 0110_0001    15      -       -       -       2/3     AJMP    code addr
// 0110_0010    4       3       3       1       2/2     XRL     direct,A
// 0110_0011    11      3       8       1       3/3     XRL     direct,#data
// 0110_0100    4       5       1       -       2/2     XRL     A,#data
// 0110_0101    4       3       1       -       2/2     XRL     A,direct
// 0110_011x    3       4       1       -       1/1     XRL     A,@Ri
// 0110_1xxx    3       2       1       -       1/1     XRL     A,Rn
//
// 0111_0000    18      1       -       -       2/3     JNZ     code addr
// 0111_0001    14      -       -       -       2/3     ACALL   code addr
// 0111_0010    6       7       -       -       2/2     ORL     C,bit addr
// 0111_0011    17      -       -       -       1/3     JMP     @A+DPTR
// 0111_0100    5       5       1       -       2/2     MOV     A,#data
// 0111_0101    10      5       8       -       3/3     MOV     direct,#data
// 0111_011x    5       5       4       -       2/2     MOV     @Ri,#data
// 0111_1xxx    5       5       2       -       2/2     MOV     Rn,#data
//
// 1000_0000    16      -       -       -       2/3     SJMP    code addr
// 1000_0001    15      -       -       -       2/3     AJMP    code addr
// 1000_0010    6       7       -       -       2/2     ANL     C,bit
// 1000_0011    19      14      5       -       1/3     MOVC    A,@A+PC
// 1000_0100    29      -       -       -       1/5     DIV     AB
// 1000_0101    9       3       3       -       3/3     MOV     direct,direct
// 1000_011x    5       4       3       -       2/2     MOV     direct,@Ri
// 1000_1xxx    5       2       3       -       2/2     MOV     direct,Rn
//
// 1001_0000    20      -       13      -       3/3     MOV     DPTR,#data
// 1001_0001    14      -       -       -       2/3     ACALL   code addr
// 1001_0010    5       7       7       1       2/2     MOV     bit,C
// 1001_0011    19      14      5       -       1/3     MOVC    A,@A+DPTR
// 1001_0100    4       5       1       -       2/2     SUBB    A,#data
// 1001_0101    4       3       1       -       2/2     SUBB    A,direct
// 1001_011x    3       4       1       -       1/1     SUBB    A,@Ri
// 1001_1xxx    3       2       1       -       1/1     SUBB    A,Rn
//
// 1010_0000    6       7       -       -       2/2     ORL     C,/bit
// 1010_0001    15      -       -       -       2/3     AJMP    code addr
// 1010_0010    6       7       -       -       2/2     MOV     C,bit
// 1010_0011    12      -       14      -       1/3     INC     DPTR
// 1010_0100    28      -       -       -       1/5     MUL     AB
// 1010_0101    0       -       -       -       -       reserved (-> NOP)
// 1010_011x    5       3       4       -       2/2     MOV     @Ri,direct
// 1010_1xxx    5       3       2       -       2/2     MOV     Rn,direct
//
// 1011_0000    6       7       -       -       2/2     ANL     C,/bit
// 1011_0001    14      -       -       -       2/3     ACALL   code addr
// 1011_0010    5       7       7       1       2/2     CPL     bit
// 1011_0011    1       -       -       -       1/1     CPL     C
// 1011_0100    22      1       -       -       3/4     CJNE   A,#data,rel
// 1011_0101    22      1       -       -       3/4     CJNE   A,direct,rel
// 1011_011x    22      4       -       -       3/4     CJNE   @Ri,#data,rel
// 1011_1xxx    22      2       -       -       3/4     CJNE   Rn,#data,rel
//
// 1100_0000    7       3       12      -       2/2     PUSH    direct
// 1100_0001    15      -       -       -       2/3     AJMP    code addr
// 1100_0010    5       7       7       1       2/2     CLR     bit
// 1100_0011    1       -       -       -       1/1     CLR     C
// 1100_0100    1       1       1       -       1/1     SWAP    A
// 1100_0101    5       3       9       -       2/2     XCH     A,direct
// 1100_011x    1       4       10      -       1/1     XCH     A,@Ri
// 1100_1xxx    1       2       9       -       1/1     XCH     A,Rn
//
//
// 0111_0000    18      1       -       -       2/3     JNZ     code addr
// 0111_0001    14      -       -       -       2/3     ACALL   code addr
// 0111_0010    6       7       -       -       2/2     ORL     C,bit addr
// 0111_0011    17      -       -       -       1/3     JMP     @A+DPTR
// 0111_0100    5       5       1       -       2/2     MOV     A,#data
// 0111_0101    10      5       8       -       3/3     MOV     direct,#data
// 0111_011x    5       5       4       -       2/2     MOV     @Ri,#data
// 0111_1xxx    5       5       2       -       2/2     MOV     Rn,#data
//
// 1000_0000    16      -       -       -       2/3     SJMP    code addr
// 1000_0001    15      -       -       -       2/3     AJMP    code addr
// 1000_0010    6       7       -       -       2/2     ANL     C,bit
// 1000_0011    19      14      5       -       1/3     MOVC    A,@A+PC
// 1000_0100    29      -       -       -       1/5     DIV     AB
// 1000_0101    9       3       3       -       3/3     MOV     direct,direct
// 1000_011x    5       4       3       -       2/2     MOV     direct,@Ri
// 1000_1xxx    5       2       3       -       2/2     MOV     direct,Rn
//
// 1001_0000    20      -       13      -       3/3     MOV     DPTR,#data
// 1001_0001    14      -       -       -       2/3     ACALL   code addr
// 1001_0010    5       7       7       1       2/2     MOV     bit,C
// 1001_0011    19      14      5       -       1/3     MOVC    A,@A+DPTR
// 1001_0100    4       5       1       -       2/2     SUBB    A,#data
// 1001_0101    4       3       1       -       2/2     SUBB    A,direct
// 1001_011x    3       4       1       -       1/1     SUBB    A,@Ri
// 1001_1xxx    3       2       1       -       1/1     SUBB    A,Rn
//
// 1010_0000    6       7       -       -       2/2     ORL     C,/bit
// 1010_0001    15      -       -       -       2/3     AJMP    code addr
// 1010_0010    6       7       -       -       2/2     MOV     C,bit
// 1010_0011    12      -       14      -       1/3     INC     DPTR
// 1010_0100    28      -       -       -       1/5     MUL     AB
// 1010_0101    0       -       -       -       -       reserved (-> NOP)
// 1010_011x    5       3       4       -       2/2     MOV     @Ri,direct
// 1010_1xxx    5       3       2       -       2/2     MOV     Rn,direct
//
// 1011_0000    6       7       -       -       2/2     ANL     C,/bit
// 1011_0001    14      -       -       -       2/3     ACALL   code addr
// 1011_0010    5       7       7       1       2/2     CPL     bit
// 1011_0011    1       -       -       -       1/1     CPL     C
// 1011_0100    22      1       -       -       3/4     CJNE   A,#data,rel
// 1011_0101    22      1       -       -       3/4     CJNE   A,direct,rel
// 1011_011x    22      4       -       -       3/4     CJNE   @Ri,#data,rel
// 1011_1xxx    22      2       -       -       3/4     CJNE   Rn,#data,rel
//
// 1100_0000    7       3       12      -       2/2     PUSH    direct
// 1100_0001    15      -       -       -       2/3     AJMP    code addr
// 1100_0010    5       7       7       1       2/2     CLR     bit
// 1100_0011    1       -       -       -       1/1     CLR     C
// 1100_0100    1       1       1       -       1/1     SWAP    A
// 1100_0101    5       3       9       -       2/2     XCH     A,direct
// 1100_011x    1       4       10      -       1/1     XCH     A,@Ri
// 1100_1xxx    1       2       9       -       1/1     XCH     A,Rn
//
// 1101_0000    7       12      3       -       2/2     POP     direct
// 1101_0001    14      -       -       -       2/3     ACALL   code addr
// 1101_0010    5       7       7       1       2/2     SETB    bit
// 1101_0011    1       -       -       -       1/1     SETB    C
// 1101_0100    1       1       1       -       1/1     DA      A
// 1101_0101    26      3       3       1       3/4     DJNZ    direct,rel
// 1101_011x    1       4       11      -       1/1     XCHD    A,@Ri
// 1101_1xxx    13      2       2       -       2/3     DJNZ    Rn,rel
//
// 1110_0000    8       15      5       -       1/2*    MOVX    A,@DPTR
// 1110_0001    15      -       -       -       2/3     AJMP    code addr
// 1110_001x    8       6       5       -       1/2*    MOVX    A,@Ri
// 1110_0100    1       1       1       -       1/1     CLR     A
// 1110_0101    5       3       1       -       2/2     MOV     A,direct
// 1110_011x    1       4       1       -       1/1     MOV     A,@Ri
// 1110_1xxx    1       2       1       -       1/1     MOV     A,Rn
//
// 1111_0000    8       -       15      -       1/2*    MOVX    @DPTR,A
// 1111_0001    14      -       -       -       2/3     ACALL   code addr
// 1111_001x    8       8       6       -       1/2*    MOVX    @Ri,A
// 1111_0100    1       1       1       -       1/1     CPL     A
// 1111_0101    5       1       3       -       2/2     MOV     direct,A
// 1111_011x    1       1       4       -       1/1     MOV     @Ri,A
// 1111_1xxx    1       1       2       -       1/1     MOV     Rn,A
//
//
//------------------------------------------------------------------------------
 
//------------------------------------------------------------------------------
wire [7:0] op;
wire int;
wire idle_mode_n;
reg  [4:0] itype;            // 0..31
reg  [2:0] last_cycle;       // 0..7
reg  [3:0] src;              // 0..15
reg  [1:0] src_cycle;        // 0..3
reg  [3:0] dest;             // 0..15
reg  [5:0] alu_op;
reg  chg_flags;
reg  rmw;

//---------------
// local signals:
//---------------
//------------------------------------------------------------------------------

  always @(op or int or idle_mode_n)
  begin : dec_proc

    last_cycle <= 1;
    src        <= 0;
    src_cycle  <= 0;
    dest       <= 0;
    alu_op     <= `alu_op_trans;
    chg_flags  <= 0;
    rmw        <= 0;
    if (int == 1)
    begin 
      itype      <=  27;
      last_cycle <=  3;
    end
    else if (idle_mode_n == 0)
    begin 
      itype      <= 2;
      last_cycle <= 0;
    end
    else
    begin
      case (op[7:4])
        4'b0000: begin
                   case (op[3:0])
                     4'b0000: begin			// NOP
                                itype      <=  0;
                                last_cycle <=  0;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// LJMP addr16
                                itype      <=  24;
                                last_cycle <=  3;
                              end
                     4'b0011: begin			// RR A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_rr;
                              end
                     4'b0100: begin			// INC A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_inc;
                              end
                     4'b0101: begin			// INC direct
                                itype      <=  5;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  3;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_inc;
                              end
                     4'b0110,				// INC @R0
                     4'b0111: begin			// INC @R1
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  4;
                                alu_op     <= `alu_op_inc;
                              end
                     default: begin			// INC [R0,R1...R7]
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  2;
                                alu_op     <= `alu_op_inc;
                              end
                   endcase
                 end
        4'b0001: begin
                   case (op[3:0])
                     4'b0000: begin			// JBC direct, rel
                                itype      <=  21;
                                last_cycle <=  3;
                                src        <=  7;
                                src_cycle  <=  1;
                                dest       <=  7;
                                alu_op     <= `alu_op_clrb;
                                rmw        <= 1'b1;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// LCALL addr16
                                itype      <=  23;
                                last_cycle <=  3;
                              end
                     4'b0011: begin			// RRC A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_rrc;
                                chg_flags  <= 1'b1;
                              end
                     4'b0100: begin			// DEC A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_dec;
                              end
                     4'b0101: begin			// DEC direct
                                itype      <=  5;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  3;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_dec;
                              end
                     4'b0110,				// DEC @R0
                     4'b0111: begin			// DEC @R1
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  4;
                                alu_op     <= `alu_op_dec;
                              end
                     default: begin			// DEC [R0,R1..R7]
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  2;
                                alu_op     <= `alu_op_dec;
                              end
                   endcase
                 end
        4'b0010: begin
                   case (op[3:0])
                     4'b0000: begin			// JB direct,rel
                                itype      <=  21;
                                last_cycle <=  3;
                                src        <=  7;
                                src_cycle  <=  1;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// RET
                                itype      <=  25;
                                last_cycle <=  3;
                              end
                     4'b0011: begin			//RL A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_rl;
                              end
                     4'b0100: begin			// ADD A, #imm8
                                itype      <=  4;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_add;
                              end
                     4'b0101: begin			//ADD A, direct
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_add;
                              end
                     4'b0110,				// ADD A, @R0
		     4'b0111: begin			// ADD A, @R1
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  1;
                                alu_op     <= `alu_op_add;
                                chg_flags  <= 1'b1;
                              end
                     default: begin			// ADD A, [R0,R1..R7]
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  1;
                                alu_op     <= `alu_op_add;
                                chg_flags  <= 1'b1;
                              end
                   endcase
                 end
        4'b0011: begin
                   case (op[3:0])				// JNB direct, rel
                     4'b0000: begin
                                itype      <=  21;
                                last_cycle <=  3;
                                src        <=  7;
                                src_cycle  <=  1;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// RETI
                                itype      <=  25;
                                last_cycle <=  3;
                              end
                     4'b0011: begin			// ADDC A, #imm8
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_rlc;
                                chg_flags  <= 1'b1;
                              end
                     4'b0100: begin			// ADDC A, direct
                                itype      <=  4;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_addc;
                              end
                     4'b0101: begin			// RLC A
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_addc;
                              end
                     4'b0110,				// ADDC A, @R0
		     4'b0111: begin			// ADDC A, @R1
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  1;
                                alu_op     <= `alu_op_addc;
                                chg_flags  <= 1'b1;
                              end
                     default: begin			// ADDC A, [R0,R1..R7]
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  1;
                                alu_op     <= `alu_op_addc;
                                chg_flags  <= 1'b1;
                              end
                   endcase
                 end
        4'b0100: begin
                   case (op[3:0])
                     4'b0000: begin			// JC rel
                                itype      <=  18;
                                last_cycle <=  2;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// ORL direct, A
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  3;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_orl;
                              end
                     4'b0011: begin			// ORL direct, #imm8
                                itype      <=  11;
                                last_cycle <=  2;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  8;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_orl;
                              end
                     4'b0100: begin			// ORL A, #imm8
                                itype      <=  4;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_orl;
                              end
                     4'b0101: begin			// ORL A, direct
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_orl;
                              end
                     4'b0110,				// ORL A, @R0
		     4'b0111: begin			// ORL A, @R1
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  1;
                                alu_op     <= `alu_op_orl;
                              end
                     default: begin			// ORL A, [R0,R1,..R7]
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  1;
                                alu_op     <= `alu_op_orl;
                              end
                   endcase
                 end
        4'b0101: begin
                   case (op[3:0])
                     4'b0000: begin			// JNC rel
                                itype      <=  18;
                                last_cycle <=  2;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// ANL direct, A
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  3;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_anl;
                              end
                     4'b0011: begin			// ANL direct, #imm8
                                itype      <=  11;
                                last_cycle <=  2;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  8;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_anl;
                              end
                     4'b0100: begin			// ANL A, #imm8
                                itype      <=  4;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_anl;
                              end
                     4'b0101: begin			// ANL A, direct
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_anl;
                              end
                     4'b0110,				// ANL A, @R0
		     4'b0111: begin			// ANL A, @R1
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  1;
                                alu_op     <= `alu_op_anl;
                              end
                     default: begin			// ANL A,[R0..R7]
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  1;
                                alu_op     <= `alu_op_anl;
                              end
                   endcase
                 end
        4'b0110: begin
                   case (op[3:0])			// JC rel_addr
                     4'b0000: begin
                                itype      <=  18;
                                last_cycle <=  2;
                                src        <=  1;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// XRL direct, A
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  3;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_xrl;
                              end
                     4'b0011: begin			// XRL direct, #imm8
                                itype      <=  11;
                                last_cycle <=  2;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  8;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_xrl;
                              end
                     4'b0100: begin			// XRL A, #imm8
                                itype      <=  4;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_xrl;
                              end
                     4'b0101: begin			// XRL A, direct
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_xrl;
                              end
                     4'b0110,				// XRL A, @R0
		     4'b0111: begin			// XRL A, @R1
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  1;
                                alu_op     <= `alu_op_xrl;
                              end
                     default: begin			// XRL A, [R0..R7]
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  1;
                                alu_op     <= `alu_op_xrl;
                              end
                   endcase
                 end
        4'b0111: begin
                   case (op[3:0])
                     4'b0000: begin			// JNZ rel
                                itype      <=  18;
                                last_cycle <=  2;
                                src        <=  1;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// ORL C, direct_bit
                                itype      <=  6;
                                src        <=  7;
                                src_cycle  <=  1;
                                alu_op     <= `alu_op_orlcb;
                              end
                     4'b0011: begin			// JMP @A+DPTR
                                itype      <=  17;
                                last_cycle <=  2;
                              end
                     4'b0100: begin			// MOV A,#imm8
                                itype      <=  5;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  1;
                              end
                     4'b0101: begin			// MOV direct, #imm8
                                itype      <=  10;
                                last_cycle <=  2;
                                src        <=  5;
                                src_cycle  <=  2;
                                dest       <=  8;
                              end
                     4'b0110,				// MOV @R0,#imm8
		     4'b0111: begin			// MOV @R1,#imm8
                                itype      <=  5;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  4;
                              end
                     default: begin			// MOV [R0..R7],#imm8
                                itype      <=  5;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  2;
                              end
                   endcase
                 end
        4'b1000: begin
                   case (op[3:0])
                     4'b0000: begin			// SJMP addr8
                                itype      <=  16;
                                last_cycle <=  2;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// ANL C, direct_bit
                                itype      <=  6;
                                src        <=  7;
                                src_cycle  <=  1;
                                alu_op     <= `alu_op_anlcb;
                              end
                     4'b0011: begin			// MOVC A, @A+PC
                                itype      <=  19;
                                last_cycle <=  2;
                                src        <=  14;
                                src_cycle  <=  1;
                                dest       <=  5;
                              end
                     4'b0100: begin			// DIV AB
                                itype      <=  29;
                                last_cycle <=  4;
                                alu_op     <= `alu_op_div;
                              end
                     4'b0101: begin			// MOV direct,direct
                                itype      <=  9;
                                last_cycle <=  2;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  3;
                              end
                     4'b0110,				// MOV direct, @R0
		     4'b0111: begin			// MOV direct, @R1
                                itype      <=  5;
                                src        <=  4;
                                dest       <=  3;
                              end
                     default: begin		    // MOV direct, [R0..R7]
                                itype      <=  5;
                                src        <=  2;
                                dest       <=  3;
                              end
                   endcase
                 end
        4'b1001: begin
                   case (op[3:0])
                     4'b0000: begin			// MOV DPTR, #imm16
                                itype      <=  20;
                                last_cycle <=  2;
                                dest       <=  13;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// MOV direct_bit,C
                                itype      <=  5;
                                src        <=  7;
                                src_cycle  <=  1;
                                dest       <=  7;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_movbc;
                              end
                     4'b0011: begin			// MOVC A, @A+DPTR
                                itype      <=  19;
                                last_cycle <=  2;
                                src        <=  14;
                                src_cycle  <=  1;
                                dest       <=  5;
                              end
                     4'b0100: begin			// SUBB A, #imm8
                                itype      <=  4;
                                src        <=  5;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_subb;
                              end
                     4'b0101: begin			// SUBB A, direct
                                itype      <=  4;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_subb;
                              end
                     4'b0110,				// SUBB A, @R0
		     4'b0111: begin			// SUBB A, @R1
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  1;
                                alu_op     <= `alu_op_subb;
                                chg_flags  <= 1'b1;
                              end
                     default: begin			// SUBB A, [R0..R7]
                                itype      <=  3;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  1;
                                alu_op     <= `alu_op_subb;
                                chg_flags  <= 1'b1;
                              end
                   endcase
                 end
        4'b1010: begin
                   case (op[3:0])
                     4'b0000: begin		    // ORL C, ~(direct_bit)
                                itype      <=  6;
                                src        <=  7;
                                src_cycle  <=  1;
                                alu_op     <= `alu_op_orlcbn;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// MOV C, direct_bit
                                itype      <=  6;
                                src        <=  7;
                                src_cycle  <=  1;
                                alu_op     <= `alu_op_movcb;
                              end
                     4'b0011: begin			// INC DPTR
                                itype      <=  12;
                                last_cycle <=  2;
                                dest       <=  14;
                              end
                     4'b0100: begin			// MUL AB
                                itype      <=  28;
                                last_cycle <=  4;
                                alu_op     <= `alu_op_mul;
                              end
                     4'b0101: begin			// reserved (= NOP)
                                itype      <=  0;
                                last_cycle <=  0;
                              end
                     4'b0110,				// MOV @R0, direct
		     4'b0111: begin			// MOV @R1, direct
                                itype      <=  5;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  4;
                              end
                     default: begin			// MOV [R0..R7], direct
                                itype      <=  5;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  2;
                              end
                   endcase
                 end
        4'b1011: begin
                   case (op[3:0])
                     4'b0000: begin		     // ANL C, ~(direct_bit)
                                itype      <=  6;
                                src        <=  7;
                                src_cycle  <=  1;
                                alu_op     <= `alu_op_anlcbn;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// CPL direct_bit
                                itype      <=  5;
                                src        <=  7;
                                src_cycle  <=  1;
                                dest       <=  7;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_cplb;
                              end
                     4'b0011: begin			// CPL C
                                itype      <=  1;
                                last_cycle <=  0;
                                alu_op     <= `alu_op_cplc;
                                chg_flags  <= 1'b1;
                              end
                     4'b0100: begin			// CJNE A, #imm8, rel
                                itype      <=  22;
                                last_cycle <=  3;
                                src        <=  1;
                              end
                     4'b0101: begin			// CJNE A, direct, rel
                                itype      <=  22;
                                last_cycle <=  3;
                                src        <=  1;
                              end
                     4'b0110,				// CJNE @R0,#imm8,rel
		     4'b0111: begin			// CJNE @R1,#imm8,rel
                                itype      <=  22;
                                last_cycle <=  3;
                                src        <=  4;
                              end
                     default: begin		// CJNE [R0..R7], #data, rel
                                itype      <=  22;
                                last_cycle <=  3;
                                src        <=  2;
                              end
                   endcase
                 end
        4'b1100: begin
                   case (op[3:0])
                     4'b0000: begin			// PUSH direct
                                itype      <=  7;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  12;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// CLR direct_bit
                                itype      <=  5;
                                src        <=  7;
                                src_cycle  <=  1;
                                dest       <=  7;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_clrb;
                              end
                     4'b0011: begin			// CLR C
                                itype      <=  1;
                                last_cycle <=  0;
                                alu_op     <= `alu_op_clrc;
                                chg_flags  <= 1'b1;
                              end
                     4'b0100: begin			// SWAP A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_swap;
                              end
                     4'b0101: begin			// XCH A, direct
                                itype      <=  5;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  9;
                              end
                     4'b0110,				// XCH A, @R0
		     4'b0111: begin			// XCH A, @R1
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  10;
                              end
                     default: begin			// XCH A,[R0..R7]
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  9;
                              end
                   endcase
                 end
        4'b1101: begin
                   case (op[3:0])
                     4'b0000: begin			// POP direct
                                itype      <=  7;
                                src        <=  12;
                                src_cycle  <=  1;
                                dest       <=  3;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010: begin			// SETB direct_bit
                                itype      <=  5;
                                src        <=  7;
                                src_cycle  <=  1;
                                dest       <=  7;
                                rmw        <= 1'b1;
                                alu_op     <= `alu_op_setbb;
                              end
                     4'b0011: begin			// SETB C
                                itype      <=  1;
                                last_cycle <=  0;
                                alu_op     <= `alu_op_setbc;
                                chg_flags  <= 1'b1;
                              end
                     4'b0100: begin			// DA A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_da;
                                chg_flags  <= 1'b1;
                              end
                     4'b0101: begin			//DJNZ direct,rel
                                itype      <=  26;
                                last_cycle <=  3;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  3;
                                alu_op     <= `alu_op_dec;
                                rmw        <= 1'b1;
                              end
                     4'b0110,				// XCHD A, @R0
		     4'b0111: begin			// XCHD A, @R1
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  11;
                              end
                     default: begin		  // DJNZ [R0..R7], rel
                                itype      <=  13;
                                last_cycle <=  2;
                                src        <=  2;
                                dest       <=  2;
                                alu_op     <= `alu_op_dec;
                              end
                   endcase
                 end
        4'b1110: begin
                   case (op[3:0])
                     4'b0000: begin			// MOVX A,@DPTR
                                itype      <=  8;
                                src        <=  15;
                                dest       <=  5;
                              end
                     4'b0001: begin			// AJMP addr11
                                itype      <=  15;
                                last_cycle <=  2;
                              end
                     4'b0010,				// MOVX A,@R0
		     4'b0011: begin			// MOVX A,@R1
                                itype      <=  8;
                                src        <=  6;
                                dest       <=  5;
                              end
                     4'b0100: begin			// CLR A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_clr;
                              end
                     4'b0101: begin			// MOV A,direct
                                itype      <=  5;
                                src        <=  3;
                                src_cycle  <=  1;
                                dest       <=  1;
                              end
                     4'b0110,				// MOV A, @R0
		     4'b0111: begin			// MOV A, @R1
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  4;
                                dest       <=  1;
                              end
                     default: begin			// MOV A,[R0..R7]
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  2;
                                dest       <=  1;
                              end
                   endcase
                 end
        4'b1111: begin
                   case (op[3:0])
                     4'b0000: begin			// MOVX @DPTR,A
                                itype      <=  8;
                                dest       <=  15;
                              end
                     4'b0001: begin			// ACALL addr11
                                itype      <=  14;
                                last_cycle <=  2;
                              end
                     4'b0010,				// MOVX @R0, A
		     4'b0011: begin			// MOVX @R1, A
                                itype      <=  8;
                                src        <=  8;
                                dest       <=  6;
                              end
                     4'b0100: begin			// CPL A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  1;
                                alu_op     <= `alu_op_cpl;
                              end
                     4'b0101: begin			// MOV direct, A
                                itype      <=  5;
                                src        <=  1;
                                dest       <=  3;
                              end
                     4'b0110,				// MOV @R0,A
		     4'b0111: begin			// MOV @R1,A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  4;
                              end
                     default: begin			// MOV [R0..R7],A
                                itype      <=  1;
                                last_cycle <=  0;
                                src        <=  1;
                                dest       <=  2;
                              end
                   endcase
                 end
        default: begin end
      endcase
    end 
 end 


endmodule
