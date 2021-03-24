// $Id: DW8051_control.v,v 1.1 1996/07/25 17:42:30 gina Exp $
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
// FILE: DW8051_control.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 main cpu control unit (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        27.10.95        Entity module created out of common
//	L.Rieder	07.05.96	port int_sfr_rd removed
//	L.Rieder	08.05.96	port sfr_wr_addr_val removed
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//			Nov 09 97	Fix for STAR 49337: sfr_rd pulses 
//					for indirect addressed RAM read
//					accesses. The direct relationship
//					between the sfr_rd and the ram_rd_n
//					signals has been broken. The two
//					are now treated as being functionally
//					distinct.
//                      Dec 3,1997      Fix for STAR 49995. Translated from
//                                      Ludwig Rieder's VHDL version.
//      		Feb 25,98       Fix for star 51723: ram_rd_n
//                                      assignments corrected. Translated from
//					Ludwig Rieder's VHDL version.
//			March 04,98	Fix for STAR 52000. This is an extension
//					of the fix for STAR 49995, extended to
//					JBC instructions.
//			May 20, 1998	Explanation for the Blocking
//					statements used for sfr_addr, sfr_rd,
//					sfr_wr and sfr_data_out:
//					  These blocking assignments are there
//					  for the sole purpose of providing
//					  separate flip-flops for
//					  int/ext_sfr_addr, int/ext_sfr_wr
//					  etc..
//			May 20, 1998	Fix for STAR 54739: Incorrect
//					implementation of the AJMP
//					instruction:  The pc_inc value in the
//					instruction sequencer has to be made
//					0x00 for this instruction, not 0x01.
//					Else, the PC increments 3 times
//					instead of 2 as specified by the
//					instruction set architecture.
//					Even with this bug, normal operation 
//					will not be affected because bits [10:0]
//				        of the PC will be overwritten anyway.
//			May 20, 1998	- Reconditioned sfr_wr as per Ludwig's
//					recommendations, in a number of places.
//					sfr_wr is now asserted, fully
//					conditioned by the value of sfr_addr[7].
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_control (// standard signals:
                       clk,
                       rst_n,
                       cycle,		// act.cycle

                       // inputs:
                       biu_instr,
                       biu_ram_access_rdy,

                       int_req,
                       int_src,
    
                       alu,
                       alu_l,
                       sp,
                       dps,
                       pcon,
                       psw,
                       acc,
                       b,
                       dptr,
                       indir_data_in,
                       pc,
                       add16_sum,
                       eie_eip_check,
                       mpage,

                       dec_itype,
                       dec_last_cycle,
                       dec_src,
                       dec_src_cycle,
                       dec_dest,
                       dec_alu_op,
                       dec_chg_flags,
                       dec_rmw,

                       alu_zero,
                       alu_equal,
                       bit_status,

                       // outputs:
                       cpu_idle_mode_n,
                       cpu_stop_mode_n,
                       biu_ram_addr,
                       biu_wr_ram_addr_h,
                       biu_wr_ram_addr_l,
                       biu_data_out,
                       biu_wr_ram,
                       biu_rd_ram,
                       biu_rd_rom,

                       ram_addr,
                       ram128_wr_addr_val_n,
                       ram256_wr_addr_val_n,
                       ram_data_in,
                       ram128_wr_n,
                       ram256_wr_n,
                       ram_rd_n,
                       int_sfr_addr,
                       int_sfr_data_out,
                       int_sfr_wr,
                       ext_sfr_addr,
                       ext_sfr_data_out,
                       ext_sfr_wr,
                       ext_sfr_rd,
                       sfr_data_in,
                       port_pin_reg_n,

                       int_ack,
                       int_clr,
                       int_rec,

                       cpu_temp1,
                       cpu_temp2,
                       cpu_temp3,
                       result,

                       pc_cnt_dir,
                       inc_pc,
                       set_pc_n,
                       pc_inc,
                       dp_inc,
                       pc_add_signed,
                       sel_pc_dptr_n,
    
                       sp_cnt_dir,
                       cnt_sp,

                       alu_op,
                       chg_flags,
                       acc_data,
                       ld_acc,
                       ld_acc_direct,

                       cpu_bit_nr
		       );

 input clk;
 input rst_n;
 input [1:0]  cycle;
 input [7:0]  biu_instr;
 input biu_ram_access_rdy;
 input int_req;
 input [3:0]  int_src;
 input [7:0]  alu;
 input [7:0]  alu_l;
 input [7:0]  sp;
 input [7:0]  dps;
 input [7:0]  pcon;
 input [7:0]  psw;
 input [7:0]  acc;
 input [7:0]  b;
 input [15:0]  dptr;
 input [7:0]  indir_data_in;
 input [15:0]  pc;
 input [15:0]  add16_sum;
 input eie_eip_check;
 input [7:0]  mpage;
 input [4:0] dec_itype;
 input [2:0] dec_last_cycle;
 input [3:0] dec_src;
 input [1:0] dec_src_cycle;
 input [3:0] dec_dest;
 input [5:0]  dec_alu_op;
 input dec_chg_flags;
 input dec_rmw;
 input alu_zero;
 input alu_equal;
 input bit_status;
 input [7:0]  sfr_data_in;
 output cpu_idle_mode_n;
 output cpu_stop_mode_n;
 output [15:0]  biu_ram_addr;
 output biu_wr_ram_addr_h;
 output biu_wr_ram_addr_l;
 output [7:0]  biu_data_out;
 output biu_wr_ram;
 output biu_rd_ram;
 output biu_rd_rom;
 output [7:0]  ram_addr;
 output ram128_wr_addr_val_n;
 output ram256_wr_addr_val_n;
 output [7:0]  ram_data_in;
 output ram128_wr_n;
 output ram256_wr_n;
 output ram_rd_n;
 output [7:0]  int_sfr_addr;
 output [7:0]  int_sfr_data_out;
 output int_sfr_wr;
 output [7:0]  ext_sfr_addr;
 output [7:0]  ext_sfr_data_out;
 output ext_sfr_wr;
 output ext_sfr_rd;
 output port_pin_reg_n;
 output int_ack;
 output int_clr;
 output int_rec;
 output [7:0]  cpu_temp1;
 output [7:0]  cpu_temp2;
 output [7:0]  cpu_temp3;
 output [15:0]  result;
 output pc_cnt_dir;
 output inc_pc;
 output set_pc_n;
 output [7:0]  pc_inc;
 output [7:0]  dp_inc;
 output pc_add_signed;
 output sel_pc_dptr_n;
 output sp_cnt_dir;
 output cnt_sp;
 output [5:0]  alu_op;
 output chg_flags;
 output [7:0]  acc_data;
 output ld_acc;
 output ld_acc_direct;
 output [7:0]  cpu_bit_nr;


//------------------------------------------------------------------------------
//                             DESCRIPTION
//------------------------------------------------------------------------------
//
// The DW8051_control module is the central module of the cpu. It controls the
// DW8051_biu (bus interface unit), the DW8051_op_decoder (opcode decoder),
// the DW8051_alu (arithmetic logical unit), handles all control flow of the
// different commands, interrupts and manages all read/write operations.
// All FF's are running on posedge of clk and reset asynchronously.
// Although there is only one main process (main_control_proc), basically there
// are managed 3 different tasks inside the process:
// a) handle all source (read) operations (source sequencer)
// b) handle all destination (write) operations (destination sequencer)
// c) handle the control flow of all opcodes (instruction sequencer)
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
//      One Cycle instructions:
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
//      Two Cycle instructions:
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
//      Three Cycle instructions:
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
//      Four Cycle instructions:
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
//      Five Cycle instructions:
// 28   MUL AB
// 29   DIV AB
//
// itypes NOT used: 30,31
//
//
// Interrupts:
//          |c1|c2|c3|c4|c1|c2|c3|c4|c1| ... |c1|c2|c3|c4|
// int_req  xxxxxxxxxxxxxxxxxxx---xxxx       xxxxxxxxxxxxx
// int_ack  ______________________---_       _____________
// int_clr  __________________________       __________---_  (RETI)
// int_rec                       ^
//
//------------------------------------------------------------------------------

wire clk;
wire rst_n;
wire [1 : 0 ] cycle;
wire [7 : 0 ] biu_instr;
wire biu_ram_access_rdy;
wire int_req;
wire [ 3:0] int_src;
wire [ 7:0] alu;
wire [ 7:0] alu_l;
wire [ 7:0] sp;
wire [ 7:0] dps;
wire [ 7:0] pcon;
wire [ 7:0] psw;
wire [ 7:0] acc;
wire [ 7:0] b;
wire [15:0] dptr;
wire [ 7:0] indir_data_in;
wire [15:0] pc;
wire [15:0] add16_sum;
wire eie_eip_check;
wire [ 7:0] mpage;
wire [ 4:0] dec_itype;		// 0..31
wire [ 2:0] dec_last_cycle;	// 0..7
wire [ 3:0] dec_src;		// 0..15
wire [ 1:0] dec_src_cycle;	// 0..3
wire [ 3:0] dec_dest;		// 0..15
wire [ 5:0] dec_alu_op;
wire dec_chg_flags;
wire dec_rmw;
wire alu_zero;
wire alu_equal;
wire bit_status;
wire [ 7:0] sfr_data_in;
wire cpu_idle_mode_n;
wire cpu_stop_mode_n;

reg  [15:0] biu_ram_addr;
reg  biu_wr_ram_addr_h;
reg  biu_wr_ram_addr_l;
reg  [ 7:0] biu_data_out;
reg  biu_wr_ram;
reg  biu_rd_ram;
reg  biu_rd_rom;
reg  [ 7:0] ram_addr;
reg  ram128_wr_addr_val_n;
reg  ram256_wr_addr_val_n;
reg  [ 7:0] ram_data_in;
reg  ram128_wr_n;
reg  ram256_wr_n;
reg  ram_rd_n;
wire [ 7:0] int_sfr_addr;
reg  [ 7:0] int_sfr_data_out;
reg  int_sfr_wr;
reg  [ 7:0] ext_sfr_addr;
reg  [ 7:0] ext_sfr_data_out;
reg  ext_sfr_wr;
reg  ext_sfr_rd;
reg  port_pin_reg_n;
wire int_ack;
reg  int_clr;
reg  int_rec;
wire [ 7:0] cpu_temp1;
wire [ 7:0] cpu_temp2;
wire [ 7:0] cpu_temp3;
reg  [15:0] result;
reg  pc_cnt_dir;
reg  inc_pc;
reg  set_pc_n;
reg  [ 7:0] pc_inc;
reg  [ 7:0] dp_inc;
reg  pc_add_signed;
reg  sel_pc_dptr_n;
reg  sp_cnt_dir;
reg  cnt_sp;
reg  [ 5:0] alu_op;
reg  chg_flags;
reg  [ 7:0] acc_data;
reg  ld_acc;
reg  ld_acc_direct;
wire [ 7:0] cpu_bit_nr;

//---------------
// local signals:
//---------------
reg  [ 4:0] itype;		// 0..31
 
reg  stop_mode_n;
reg  idle_mode_n;
reg  idle_mode_del_n;
 
reg  [ 7:0] t_sfr_addr;
reg  [ 7:0] indir_addr;
wire [ 7:0] sfr_bit_addr;
reg  [ 7:0] xch_addr;
reg  [ 1:0] rs;			// Register select
 
reg  [ 7:0] act_instr;
reg  [ 2:0] instr_cycle;	// 0..7
 
reg  [ 7:0] temp1;
reg  [ 7:0] temp2;
reg  [ 7:0] temp3;
 
reg  [ 2:0] last_cycle;		// 0..7
reg  [ 3:0] src;		// 0..15
reg  [ 1:0] src_cycle;		// 0..3
reg  [ 3:0] dest;		// 0..15
 
wire [15:0] new_pc;
reg  auto_inc_pc;
reg  [ 3:0] t_dest;		// 0..15
reg  wait_for_ram;
reg  int_delay;
reg  [ 3:0] int_src_rec;
reg  t_int_ack;
 
// signals for bit operations:
reg  [ 7:0] bit_nr;
 

// local variables for main_control_proc:
reg [ 7:0] sfr_addr;
reg [ 7:0] sfr_data_out;
reg sfr_wr;
reg sfr_rd;

//------------------------------------------------------------------------------
// The reg types "sfr_rd", "sfr_wr", "sfr_addr[7:0]", "sfr_data_out[7:0]" are
// used in blocking styles throughout.  In the same clocked process that these
// regs are assigned in a blocking fashion, other signals are also assigned to
// these regs.  
// The main purpose of the blocking style is as follows: each of these regs
// (e.g., sfr_wr), is assiged to TWO signals (ext_sfr_wr and int_sfr_wr) in
// the same clocked process.  Therefore, the easiest way to duplicate the
// flip-flops for the TWO signals is to use an intermediate blocking
// variable, and assign it to other signals in the *SAME* clocked process.
// The other alternative is to replace *EACH* occurrence of the blocking
// assignment of sfr_wr with TWO non-blocking assignments, one for int_sfr_wr
// and the other for ext_sfr_wr.  
//------------------------------------------------------------------------------

  assign  new_pc  = add16_sum;

  always @(posedge clk or negedge rst_n)
  begin : main_control_proc
    if (rst_n == 0)
    begin 
      idle_mode_n          <= 1;
      idle_mode_del_n      <= 1;
      stop_mode_n          <= 1;
      biu_ram_addr         <= 'b0;
      biu_wr_ram_addr_h    <= 0;
      biu_wr_ram_addr_l    <= 0;
      biu_data_out         <= 'b0;
      biu_wr_ram           <= 0;
      biu_rd_ram           <= 0;
      biu_rd_rom           <= 0;
      wait_for_ram         <= 0;
      sfr_addr              = 0;
      sfr_data_out          = 0;
      sfr_wr                = 0;
      sfr_rd                = 0;
      ram_addr             <= 'b0;
      ram128_wr_addr_val_n <= 1;
      ram256_wr_addr_val_n <= 1;
      ram_data_in          <= 'b0;
      ram128_wr_n          <= 1;
      ram256_wr_n          <= 1;
      ram_rd_n             <= 1;
      t_sfr_addr           <= 'b0;
      int_sfr_data_out     <= 'b0;
      int_sfr_wr           <= 0;
      ext_sfr_addr         <= 'b0;
      ext_sfr_data_out     <= 'b0;
      ext_sfr_wr           <= 0;
      ext_sfr_rd           <= 0;
      port_pin_reg_n       <= 0;
      indir_addr           <= 'b0;
      t_int_ack            <= 0;
      int_clr              <= 0;
      int_delay            <= 0;
      int_rec              <= 0;
      int_src_rec          <= 'b0;
      itype                <= 0;
      last_cycle           <= 0;
      src                  <= 0;
      src_cycle            <= 0;
      dest                 <= 0;
      auto_inc_pc          <= 1;
      t_dest               <=  0;
      xch_addr             <= 'b0;
      rs                   <= 'b0;
      act_instr            <= 'b0;
      instr_cycle          <=  0;
      temp1                <= 'b0;
      temp2                <= 'b0;
      temp3                <= 'b0;
      result               <= 'b0;
      pc_cnt_dir           <= 1;		// default pc dir: up
      inc_pc               <= 0;
      set_pc_n             <= 1;
      pc_inc               <= 'b0;
      dp_inc               <= 'b0;
      pc_add_signed        <= 1;
      sel_pc_dptr_n        <= 1;
      sp_cnt_dir           <= 1;		//default sp dir: up
      cnt_sp               <= 0;
      alu_op               <= 'b0;
      chg_flags            <= 0;
      acc_data             <= 'b0;
      ld_acc               <= 0;
      ld_acc_direct        <= 0;
      bit_nr               <= 'b0;
    end
    else	// posedge clk
    begin

      idle_mode_del_n <= idle_mode_n;		// simple delay

      //----------------------------------------
      // Finish read of Ri (if instr_cycle = 0):
      //----------------------------------------
      if ((instr_cycle == 0 ) && (cycle == `c2 ))
      begin 
        indir_addr  <= sfr_data_in;
        ram_rd_n    <= 1;
      end 

      //----------------------------
      // source sequencer:
      // source is read generally in
      // c1/c2 of a instr_cycle
      //----------------------------
      if (src_cycle == instr_cycle)
      begin 
        case (src)
          // 1: src accu
          1 : begin
                case (cycle)
                  `c3 : temp1  <= acc;		// src accumulator
                  default: begin end
                endcase
              end
          // 2: src register
          // 8: src register for MOVX @Ri
          2, 8 : begin				// src register
                   case (cycle)
                     `c2 : begin
                             if (src == 2)
                             begin 
                               sfr_addr  = {3'b000, rs, biu_instr[2:0]};
                               xch_addr <= {3'b000, rs, biu_instr[2:0]};
                             end
                             else
                             begin 
                               // MOVX @Ri,A
                               sfr_addr  = {3'b000, rs, 2'b00, biu_instr[0]};
                             end
			     ram_rd_n <= 0;	// must be in RAM
                           end
                     `c3 : begin
			     ram_rd_n <= 1;
                             temp1  <= sfr_data_in;
                           end
                     default: begin end
                   endcase
                 end
          // 3: src direct, src_cycle = 1
          3 : begin
                case (cycle)
                  `c1 : begin
                          xch_addr <= biu_instr;
                        end
                  `c2 : begin
                          sfr_addr = biu_instr;		// dir.addr in 2nd Byte
			  if (biu_instr[7] == 1)  
			  begin                 // Upper 128 bytes direct-
                            sfr_rd   = 1;	// accessed using sfr_rd
			  end
			  else begin		// Lower 128 bytes direct-
			    ram_rd_n <= 0;	// accessed using ram_rd_n
			  end
                        end
                  `c3 : begin
                          sfr_rd  = 0;
			  ram_rd_n <= 1;
                          temp1  <= sfr_data_in;
                        end
                  default: begin end
                endcase
              end
          //  4: src indirect, src_cycle = 0
          4 : begin
                case (cycle)
                  `c2 : begin
                          sfr_addr  = sfr_data_in;	// R0/1 content
                          xch_addr <= sfr_data_in;
			  ram_rd_n <= 0;	//indirect read 
						// only using ram_rd_n
                        end
                  `c3 : begin
			  ram_rd_n <= 1;
                          temp1  <= indir_data_in;
                        end
                  default: begin end
                endcase
              end
          // 5: src immediate
          5 : begin
                case (cycle)
                  `c3 : temp1 <= biu_instr;
                  default: begin end
                endcase
              end
          //  6: src ext.RAM, @Ri
          6 : begin
                case (cycle)
                  `c3 : begin
                          biu_ram_addr      <= {mpage, indir_addr};
                          biu_wr_ram_addr_l <= 1;	// 8 bit
                          biu_rd_ram        <= 1;
                        end
                  `c4 : begin
                          biu_wr_ram_addr_l <= 0;
                        end
                  default: begin end
                endcase
              end
          // 7: src bit, src_cycle = 1
          7 : begin
                case (cycle)
                  `c1 : begin
                          // save bit number for later (re)write:
                          bit_nr <= biu_instr;
                        end
                  `c2 : begin
                          if (biu_instr[7]== 0)
			  begin
                           // area 20..2F
                            sfr_addr = {4'b0010, biu_instr [6:3]};
			    ram_rd_n <= 0;	//accessed using ram_rd_n
			  end
                          else 
			  begin
                            // SFR's
                            sfr_addr = {biu_instr [7:3], 3'b000};
			    sfr_rd = 1;		//SFRs are accessed using sfr_rd
                          end 
                        end
                  `c3 : begin
                          sfr_rd  = 0;
			  ram_rd_n <= 1;
                          temp1  <= sfr_data_in;
                        end
                  default: begin end
                endcase
              end
          // 12: source @SP
          12 : begin
                 case (cycle)
                   `c1 : begin
                           sfr_addr = sp;
			   ram_rd_n <= 0;	//  only ram_rd_n can be used
                         end
                   `c2 : begin
			   ram_rd_n <= 1;
                           temp1  <= indir_data_in;
                         end
                   default: begin end
                 endcase
               end
          // 14: src ROM
          14 : begin
                 case (cycle)
                   `c3 : begin
                           biu_ram_addr      <= add16_sum;
                           biu_wr_ram_addr_h <= 1;	// 16 bit
                           biu_wr_ram_addr_l <= 1;
                           biu_rd_rom        <= 1;
                         end
                   `c4 : begin
                           biu_wr_ram_addr_h <= 0;
                           biu_wr_ram_addr_l <= 0;
                         end
                   default: begin end
                 endcase
               end
          // 15: src ext.RAM, @DPTR
          15 : begin
                 case (cycle)
                   `c3 : begin
                           biu_ram_addr      <= dptr;
                           biu_wr_ram_addr_h <= 1;	// 16 bit
                           biu_wr_ram_addr_l <= 1;
                           biu_rd_ram        <= 1;
                         end
                   `c4 : begin
                           biu_wr_ram_addr_h <= 0;
                           biu_wr_ram_addr_l <= 0;
                         end
                   default: begin end
                 endcase
               end
          default: begin end
        endcase
      end  // source sequencer



      //--------------------------------------------------------------
      // destination sequencer:
      // dest should be set latest in c2 !
      // Data is written always in c1 (of the next instruction cycle).
      // For an indirect write the address is fetched in c4.
      // sfr_data_out must be set at last in c4, in c1 there may be new
      // data (new instr).
      //--------------------------------------------------------------
      case (dest)
        // 1: dest accumulator
        1 : begin
              case (cycle)
                `c3 : begin
                        sfr_addr = `acc_addr;
                      end
                `c4 : begin
                        sfr_data_out = alu;
                        sfr_wr       = 1;
                      end
                default: begin end
              endcase
            end
        // 2: dest register
        2 : begin
              case (cycle)
                `c3 : begin
                        sfr_addr = {3'b000, rs, act_instr[2:0]};
                        ram128_wr_addr_val_n <= 0;
                      end
                `c4 : begin
                        sfr_data_out  = alu;
                        ram128_wr_n  <= 0;
                      end
                default: begin end
              endcase
            end
        // 3: dest direct
        3 : begin
              case (cycle)
                `c3 : begin
                        sfr_addr = biu_instr;
                        if (biu_instr[7] == 0)
                          ram128_wr_addr_val_n <= 0;
                      end
                `c4 : begin
                        sfr_data_out = alu;
                        if (sfr_addr[7] == 0)		//  sfr_addr ! for
                        begin				// exit of idle_mode
                          ram128_wr_n <= 0;
                        end 
			else
                          sfr_wr = 1;			

                        // check for access to PCON (idle/stop mode)
                        // and IE/IP/EIE/EIP (for int_delay, done
                        // here also for DJNZ)
                        // PSW check also here
                        case (biu_instr )
                          `psw_addr : rs <= alu[4:3];
                          `pcon_addr: begin
                                        if (alu[0] == 1)
                                        begin 
                                          if (t_int_ack == 1)
                                            // avoid set of idle_mode:
                                            sfr_data_out = {alu[7:1], 1'b0};
                                          else
                                            idle_mode_n <= 0;
                                        end 
                                        if (alu[1] == 1)
                                          stop_mode_n <= 0;
                                      end
                          `ie_addr,
                          `ip_addr  : begin
                                        if (act_instr == 8'b11010101)
                                          // DJNZ direct,rel
                                          int_delay <= 1;
                                      end
                          `eie_addr,
                          `eip_addr : begin
                                        if ((eie_eip_check == 1) &
                                            (act_instr == 8'b11010101))
                                          // DJNZ direct,rel
                                          int_delay <= 1;
                                      end
                          default: begin end
                        endcase
                      end
                default: begin end
              endcase
            end
        // 4: dest indirect
        4 : begin
              case (cycle)
                `c3 : begin
                        sfr_addr = indir_addr;
                        if (indir_addr[7] == 0)
                          ram128_wr_addr_val_n <= 0;
                        else
                          ram256_wr_addr_val_n <= 0;
                      end
                `c4 : begin
                        sfr_data_out = alu;
                        if (indir_addr[7] == 0)
                          ram128_wr_n <= 0;
                        else
                          ram256_wr_n <= 0;
                      end
                default: begin end
              endcase
            end
        // 5: dest accumulator direct (src biu_data_in)
        5 : begin
              case (cycle)
                `c4 : ld_acc_direct <= 1;
                default: begin end
              endcase
            end
        //  6: dest ext.RAM, @Ri
        6 : begin
              case (instr_cycle)
                0 : begin
                      case (cycle)
                        `c3 : begin
                                biu_wr_ram_addr_l <= 1;		// 8 bit
                                biu_wr_ram        <= 1;
                              end
                        `c4 : begin
                                biu_wr_ram_addr_l <= 0;
                                biu_ram_addr      <= {mpage, alu};
                                biu_data_out      <= acc;
                              end
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        // 7: dest bit, instr_cycle=1
        7 : begin
              case (instr_cycle)
                1,
                2 : begin
                      case (cycle)
                        `c3 : begin
                                if (bit_nr[7] == 0)
                                begin 
                                  // area 20..2F
                                  sfr_addr = {4'b0010, bit_nr[6:3]};
                                  ram128_wr_addr_val_n <= 0;
                                end
                                else
                                  // SFR's
                                  sfr_addr = {bit_nr[7:3], 3'b000};
                              end
                        `c4 : begin
                                sfr_data_out = alu;
                                if (bit_nr[7] == 0)
                                begin 
                                  ram128_wr_n <= 0;
                                end 
				else 
                                  sfr_wr = 1;

                                // check for access to PSW:
                                if (sfr_bit_addr == `psw_addr)
                                  rs <= alu[4:3];
                              end
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        // 8: dest direct, address in 2nd Byte, data in 3rd Byte
        8 : begin
              case (instr_cycle)
                1 : begin
                      case (cycle)
                        `c3 : sfr_addr = biu_instr;
                        default: begin end
                      endcase
                    end
                2 : begin
                      case (cycle)
                        `c3 : begin
                                if (sfr_addr[7] == 0)
                                  ram128_wr_addr_val_n <= 0;
                              end
                        `c4 : begin
                                sfr_data_out = alu;
                                if (sfr_addr[7] == 0)
                                begin 
                                  ram128_wr_n <= 0;
                                end 
                                else 
				  sfr_wr = 1;

                                // check for access to PCON and PSW:
                                case (t_sfr_addr )
                                  `psw_addr  : rs <= alu[4:3];
                                  `pcon_addr : begin
                                                 if (alu[0] == 1)
                                                 begin 
                                                   if (t_int_ack == 1)
                                                     // avoid set of idle_mode:
                                                     sfr_data_out = {alu[7:1],
                                                                     1'b0};
                                                   else
                                                     idle_mode_n <= 0;
                                                 end 
                                                 if (alu[1] == 1)
                                                   stop_mode_n <= 0;
                                               end
                                  default: begin end
                                endcase
                              end
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        // 9: dest direct, data in acc, address xch_addr
        9 : begin
              case (cycle)
                `c3 : begin
                        sfr_addr = xch_addr;
                        if (xch_addr[7] == 0)
                          ram128_wr_addr_val_n <= 0;
                      end
                `c4 : begin
                        sfr_data_out = acc;
                        if (xch_addr[7] == 0)
                        begin 
                          ram128_wr_n <= 0;
                        end 
                        else 
			  sfr_wr    = 1;
                        acc_data <= alu;
                        ld_acc   <= 1;

                        // check for access to PCON (idle/stop mode)
                        // and PSW (rs)
                        case (xch_addr)
                          `psw_addr  : rs <= temp2[4:3];
                          `pcon_addr : begin
                                         if (acc[0] == 1)
                                         begin 
                                           if (t_int_ack == 1)
                                             // avoid set of idle_mode:
                                             sfr_data_out = {acc[7:1], 1'b0};
                                           else
                                             idle_mode_n <= 0;
                                         end 
                                         if (acc[1] == 1)
                                           stop_mode_n <= 0;
                                       end
                          default: begin end
                        endcase
                      end
                default: begin end
              endcase
            end
        // 10: dest indirect, data in acc, address xch_addr
        10 : begin
               case (cycle)
                 `c3 : begin
                         sfr_addr = xch_addr;
                         if (xch_addr[7] == 0)
                           ram128_wr_addr_val_n <= 0;
                         else
                           ram256_wr_addr_val_n <= 0;
                       end
                 `c4 : begin
                         sfr_data_out = acc;
                         if (xch_addr[7] == 0)
                           ram128_wr_n <= 0;
                         else
                         begin
                           ram256_wr_n <= 0;
                         end 
                         acc_data <= alu;
                         ld_acc   <= 1;
                       end
                 default: begin end
               endcase
             end
        // 11: dest indirect, data(7:4) in alu, data(3:0) in acc,
        // address xch_addr
        11 : begin
               case (cycle)
                 `c3 : begin
                         sfr_addr = xch_addr;
                         if (xch_addr[7] == 0)
                           ram128_wr_addr_val_n <= 0;
                         else
                           ram256_wr_addr_val_n <= 0;
                       end
                 `c4 : begin
                         sfr_data_out = {alu[7:4], acc[3:0]};
                         if (xch_addr[7] == 0)
                           ram128_wr_n <= 0;
                         else
                          begin
                           ram256_wr_n <= 0;
                         end 
                         // load accu direct:
                         acc_data <= {acc[7:4], alu[3:0]};
                         ld_acc   <= 1;
                       end
                 default: begin end
               endcase
             end
        // 12: dest @SP
        12 : begin
               case (cycle)
                 `c3 : begin
                         sfr_addr = sp;
                         if (sp[7] == 0)
                           ram128_wr_addr_val_n <= 0;
                         else
                           ram256_wr_addr_val_n <= 0;
                       end
                 `c4 : begin
                         sfr_data_out = alu;
                         if (sp[7] == 0)
                           ram128_wr_n <= 0;
                         else
                           ram256_wr_n <= 0;
                       end
                 default: begin end
               endcase
             end
        // 13: dest dptr
        13,
        14 : begin
               case (instr_cycle)
                 1 : begin
                       // write to dph (dps(0)=0) or dph1 (dps(0)=1):
                       case (cycle)
                         `c3 : begin
                                 sfr_addr = {5'b10000,
                                             (1'b0 ^ dps[0]),
                                             (1'b1 ^ dps[0]), 1'b1};
                               end
                          `c4 : begin
                                 if (dest == 13)
                                   sfr_data_out = biu_instr;
                                 else
                                 begin
                                   // INC DPTR
                                   sfr_data_out = add16_sum[15:8];
                                 end 
                                 sfr_wr = 1;
                               end
                         default: begin end
                       endcase
                     end
                 2 : begin
                       // write to dpl (dps(0)=0) or dpl1 (dps(0)=1):
                       case (cycle)
                         `c3 : begin
                                 sfr_addr = {5'b10000,
                                             (1'b0 ^ dps[0]),
                                             (1'b1 ^ dps[0]), 1'b0};
                               end
                         `c4 : begin
                                 if (dest == 13)
                                   sfr_data_out = biu_instr;
                                 else
                                 begin
                                   // INC DPTR
                                   sfr_data_out = add16_sum[7:0];
                                 end 
                                 sfr_wr = 1;
                                end
                         default: begin end
                       endcase
                    end
                 default: begin end
               endcase
             end
        // 15: dest ext.RAM, @DPTR
        15 : begin
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c3 : begin
                                 biu_wr_ram_addr_h <= 1;
                                 biu_wr_ram_addr_l <= 1;
                                 biu_wr_ram        <= 1;
                               end
                         `c4 : begin
                                 biu_wr_ram_addr_h <= 0;
                                 biu_wr_ram_addr_l <= 0;
                                 biu_ram_addr      <= dptr;
                                 biu_data_out      <= acc;
                               end
                         default: begin end
                       endcase
                     end
                  default: begin end
               endcase
             end
        default: begin end
      endcase
 
 

      //------------------------------------------
      // Miscellaneous actions depending on cycle:
      //------------------------------------------
      if (cycle == `c1)
      begin 
        sfr_wr                = 0;		// finish write cycle (all dest)
        ram128_wr_n          <= 1;
        ram256_wr_n          <= 1;
        ram128_wr_addr_val_n <= 1;
        ram256_wr_addr_val_n <= 1;
        ld_acc               <= 0;
        ld_acc_direct        <= 0;
        dest                 <= 0;
        sel_pc_dptr_n        <= 1;		// default: pc
        pc_add_signed        <= 1;		// default: unsigned
        chg_flags            <= 0;
      end 

      if (cycle == `c4)
      begin 
        inc_pc     <= 0;			// done always in c4
        pc_cnt_dir <= 1;			// back to default (up) any way
      end 



      //----------------------
      // for most instructions
      // pc is incremented
      // automatically in c4:
      //----------------------
      if (auto_inc_pc == 1)
      begin 
        case (cycle)
          `c3 : begin
                  // late check for interrupts:
                  if ((instr_cycle == last_cycle) && (int_req == 1) &&
                      (int_delay == 0))
                    inc_pc <= 0;
                  else if (idle_mode_n == 0)
                    inc_pc <= 0;
                  else
                    inc_pc <= 1;
                end
          default: begin end
        endcase
      end 


      //-----------------------
      // increment instr_cycle:
      //-----------------------
      if ((cycle == `c4 ) & (wait_for_ram == 0))
      begin 
        instr_cycle <= (instr_cycle + 1);
      end 


      //-----------------------------------
      // Finish execution in c3 if
      // instr_cycle = last_cycle.
      // Interrupts are recognized here !!!
      //-----------------------------------
      if (instr_cycle == last_cycle)
      begin 
        if ((wait_for_ram == 0) | (biu_ram_access_rdy == 1))
        begin 
          // if <no MOVX @DPTR> or <MOVX @DPTR and ready)
          case (cycle)
            `c2 : begin
                    auto_inc_pc <= 1;	      // may by overridden in next cyle

                    //-------------------------------------
                    // check if interrupt has to be delayed
                    // (write access to IE,IP,EIE,EIP):
                    //-------------------------------------
                    if (t_dest == 3)
                    begin 
                      case (biu_instr)
                        `ie_addr,
                        `ip_addr  : int_delay <= 1;
                        `eie_addr,
                        `eip_addr : begin
                                      if (eie_eip_check == 1)
                                        int_delay <= 1;
                                    end
                        default  : begin end
                      endcase
                    end 
                    if (t_dest == 7)
                    begin 
                      case (sfr_bit_addr)
                        `ie_addr,
                        `ip_addr  : int_delay <= 1;
                        `eie_addr,
                        `eip_addr : begin
                                      if (eie_eip_check == 1)
                                        int_delay <= 1;
                                    end
                        default  : begin end
                      endcase
                    end 
                    if (t_dest == 8)
                    begin 
                      case (t_sfr_addr)
                        `ie_addr,
                        `ip_addr  : int_delay <= 1;
                        `eie_addr,
                        `eip_addr : begin
                                      if (eie_eip_check == 1)
                                        int_delay <= 1;
                                    end
                        default  : begin end
                      endcase
                    end 
                    if (t_dest == 9)
                    begin 
                      case (xch_addr)
                        `ie_addr,
                        `ip_addr  : int_delay <= 1;
                        `eie_addr,
                        `eip_addr : begin
                                      if (eie_eip_check == 1)
                                        int_delay <= 1;
                                    end
                        default  : begin end
                      endcase
                    end 
                    // check for RETI:
                    if (act_instr == 8'b00110010)
                    begin 
                      int_delay <= 1;
                    end 
                  end
            `c3 : begin
                    // step to next instruction, also check for interrupts:
                    itype <= 0;
                    if ((int_req == 1) && (int_delay == 0))
                    begin 
                      // accept interrupt request
                      auto_inc_pc <= 0;			// stay at last pc addr
                      int_rec     <= 1;
                      t_int_ack   <= 1;
                      // if last instruction was MOVX,
                      // decrement PC (discard read instruction) !
                      if (biu_ram_access_rdy == 1)
                      begin 
                        pc_cnt_dir <= 0;		// down
                        inc_pc     <= 1;
                      end 
                      // interrupt in idle mode ?
                      if (idle_mode_n == 0)
                      begin 
                        idle_mode_n <= 1;
                        // clear pcon(0):
                        sfr_addr     = `pcon_addr;
                        temp1       <= {pcon[7:1], 1'b0};
                        dest        <= 3;		// execute next
                        // decrement pc:
                        pc_cnt_dir  <= 0;		// down
                        inc_pc      <= 1;
                      end 
                    end
                    else if (idle_mode_n == 0)
                      auto_inc_pc <= 0;			// stay here until int
                    else
                    begin
                      auto_inc_pc <= 1;
                      int_rec     <= 0;
                    end 
                    int_delay <= 0;			// clear delay flag
                  end
            default: begin end
          endcase
        end 
      end 



      //-----------------------
      // instruction sequencer:
      // new instructions are
      // decoded always in c1
      //-----------------------
      case (itype)
        0 : begin					// wait for next instr.
              case (cycle)
                `c4 : begin
                        int_src_rec <= int_src;		// save int_src
                        t_int_ack   <= 0;
                      end
                `c1 : begin
                        itype          <= dec_itype;
                        last_cycle     <= dec_last_cycle;
                        act_instr      <= biu_instr;
                        src            <= dec_src;
                        src_cycle      <= dec_src_cycle;
                        t_dest         <= dec_dest;
                        alu_op         <= dec_alu_op;
                        chg_flags      <= dec_chg_flags;
                        port_pin_reg_n <= (~dec_rmw);
                        auto_inc_pc    <= 1;
                        instr_cycle    <= 0;
                        biu_wr_ram     <= 0;
                        biu_rd_ram     <= 0;
                        biu_rd_rom     <= 0;

                        // Unconditional read of Ri, used for
                        // instructions with @Ri (source/destination):
                        sfr_addr = {3'b000, rs, 2'b00, biu_instr[0]};
			ram_rd_n <= 0;
                      end
                default: begin end
              endcase
            end
     // one cycle instructions:
        1 : begin					// Most 1 Byte's
              case (cycle)
                `c2 : dest  <= t_dest;
                default: begin end
              endcase
            end
        2 : begin					// IDLE MODE
            end
        3 : begin					// ANL,ORL,XRL (1 Byte)
              case (cycle)				// ADD,ADDC,SUBB
                `c2 : begin
                        temp2 <= acc;
                        dest  <= t_dest;
                      end
                default: begin end
              endcase
            end
     // two cycle instructions:
        4 : begin					// ANL,ORL,XRL (2 Bytes)
              case (instr_cycle)			// ADD,ADDC,SUBB
                1 : begin
                      case (cycle)
                        `c2 : begin
                                temp2 <= acc;
                                dest  <= t_dest;
                                if (act_instr[6] == 0)
                                begin 
                                  // ADD,ADDC,SUBB
                                  chg_flags  <= 1;
                                end 
                              end
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        5 : begin					// Most 2 Byte's
              case (instr_cycle)
                1 : begin
                      case (cycle)
                        `c2 : dest <= t_dest;
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        6 : begin					// ORL C,bit, ..
              case (instr_cycle)
                1 : begin
                      case (cycle)
                        `c2 : begin
                                dest      <= t_dest;
                                chg_flags <= 1;		// update C
                              end
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        7 : begin					// PUSH,POP
              case (instr_cycle)
                0 : begin
                      case (cycle)
                        `c2 : begin
                                if (act_instr[4] == 0)
                                begin 
                                  // PUSH, (SP) <- (SP) + 1
                                  sp_cnt_dir <= 1;	// up
                                  cnt_sp     <= 1;
                                end 
                              end
                        `c3 : cnt_sp <= 0;
                        default: begin end
                      endcase
                    end
                1 : begin
                      case (cycle)
                        `c2 : begin
                                dest <= t_dest;
                                if (act_instr[4] == 1)
                                begin 
                                  // POP, (SP) <- (SP) - 1
                                  sp_cnt_dir <= 0;	// down
                                  cnt_sp     <= 1;
                                end 
                              end
                        `c3 : cnt_sp <= 0;
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        8 : begin				// MOVX @DPTR,A / MOVX A,@DPTR
              case (instr_cycle)		// MOVX @R1,A   / MOVX A,@Ri
                0 : begin
                      case (cycle)
                        `c2 : begin
                                if (act_instr[4] == 1)
                                begin
                                  // MOVX @DPTR,A / MOVX @Ri,A
                                  dest <= t_dest;
                                end 
                              end
                        default: begin end
                      endcase
                    end
                1 : begin
                      case (cycle)
                        `c2 : begin
                                wait_for_ram <= 1;	// stay here
                                auto_inc_pc  <= 0;
                              end
                        `c3 : begin
                                if (biu_ram_access_rdy == 1)
                                begin 
                                  if (act_instr[4] == 0)
                                  begin 
                                    // MOVX A,@DPTR / MOVX A,@Ri
                                    // set of dest allowed here
                                    dest <= t_dest;
                                  end 
                                  wait_for_ram <= 0;
                                end 
                              end
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
     // three cycle instructions:
        9 : begin				// MOV direct,direct
              case (instr_cycle)		// (3 Bytes)
                2 : begin
                      case (cycle)
                        `c2 : dest <= t_dest;
                        default: begin end
                      endcase
                    end
                default: begin end
              endcase
            end
        10 : begin				// MOV direct,#data
               case (instr_cycle)		// (3 Bytes)
                 1 : begin
                       case (cycle )
                         `c2 : dest <= t_dest;
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle )
                         `c2 : dest <= t_dest;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        11 : begin					// ANL,ORL,XRL (3 Bytes)
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c2 : dest  <= t_dest;
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c2 : begin
                                 temp2 <= biu_instr;    // #data
                                 dest  <= t_dest;
                                 sfr_rd = 1;            // STAR 49995 fix
				 // read in RAM is not necessary, cannot
				 // have changed meanwhile.
                               end
                         `c3 : begin                    // STAR 49995 fix
                                 sfr_rd = 0;            // STAR 49995 fix
                                 temp1 <= sfr_data_in;  // STAR 49995 fix
                               end
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        12 : begin					// INC DPTR
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : begin
                                 auto_inc_pc <= 0;
                                 dp_inc      <= 8'b00000001;
                               end
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c2 : begin
                                 sel_pc_dptr_n <= 0;	// dptr
                                 dest          <= t_dest;
                               end
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c2 : begin
                                 sel_pc_dptr_n <= 0;	// dptr
                                 dest          <= t_dest;
                               end
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        13 : begin					// DNJZ Rn,rel
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : dest  <= t_dest;
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c1 : pc_inc  <= biu_instr;
                         `c2 : auto_inc_pc  <= 0;
                         `c3 : begin
                                 if (alu_zero == 0)
                                 begin 
                                   result   <= new_pc;
                                   set_pc_n <= 0;
                                 end 
                               end
                         `c4 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        14 : begin					// ACALL
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         // increment sp before writing to @sp:
                         `c2 : begin
                                 sp_cnt_dir <= 1;		// up
                                 cnt_sp     <= 1;
                                 pc_inc     <= 8'b00000001;	// 1
                               end
                         `c3 : begin
                                 cnt_sp <= 0;
                                 temp1  <= new_pc[7:0];
                               end
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c1 : pc_inc <= 8'b00000000;		// 0
                         `c2 : dest <=  12;			// @sp
                         `c3 : begin
                                 result[15:11] <= new_pc[15:11];
                                 result[10: 8] <= act_instr[7:5];
                                 result[ 7: 0] <= biu_instr;
                                 set_pc_n      <= 0;
                               end
                         `c4 : begin
                                 set_pc_n <= 1;
                                 temp1    <= new_pc[15:8];
                               end
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c1 : cnt_sp  <= 1;
                         `c2 : begin
                                 cnt_sp <= 0;
                                 dest   <= 12;		// @sp
                               end
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        15 : begin					// AJMP
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c2 : pc_inc  <= 8'b00000000;	// STAR 54739 fix.
                         `c3 : begin
                                 result[15:11] <= new_pc[15:11];
                                 result[10: 8] <= act_instr[7:5];
                                 result[ 7: 0] <= biu_instr;
                                 set_pc_n      <= 0;
                               end
                         `c4 : begin
                                 set_pc_n <= 1;
                               end
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        16 : begin					// SJMP
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c1 : pc_inc      <= biu_instr;
                         `c2 : auto_inc_pc <= 0;
                         `c3 : begin
                                 result   <= new_pc;
                                 set_pc_n <= 0;
                               end
                         `c4 : set_pc_n  <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        17 : begin					// JMP @A+DPTR
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : auto_inc_pc <= 0;	// stay
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c2 : begin
                                 sel_pc_dptr_n <= 0;	// dptr
                                 dp_inc        <= acc;
                               end
                         `c3 : begin
                                 result   <= new_pc;
                                 set_pc_n <= 0;
                               end
                         `c4 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        18 : begin					// JNZ,JZ,JC,JNC
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c1 : pc_inc  <= biu_instr;
                         `c2 : auto_inc_pc  <= 0;
                         `c3 : begin
                                 if ( ((act_instr[5] == 1) &&
                                       (act_instr[4] != alu_zero)) ||
                                       // (JZ,JNZ)
                                      ((act_instr[5] == 0) &&
                                       (act_instr[4] != psw[7])))
                                       // (JC,JNC)
                                 begin 
                                   result   <= new_pc;
                                   set_pc_n <= 0;
                                 end 
                               end
                         `c4 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        19 : begin					// MOVC
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : auto_inc_pc <= 0;
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c2 : begin
                                 if (act_instr[4] == 1)
                                 begin 
                                   // MOVC A,@A+DPTR
                                   sel_pc_dptr_n <= 0;
                                   dp_inc        <= acc;
                                 end
                                 else
                                 begin
                                   // MOVC A,@A+PC
                                   sel_pc_dptr_n <= 1;
                                   pc_add_signed <= 0;	// unsigned
                                   pc_inc        <= acc;
                                 end 
                               end
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c2 : dest <= t_dest;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        20 : begin					//  MOV DPTR,#data16
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c2 : dest <= t_dest;
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c2 : dest <= t_dest;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
     // four cycle instructions:
        21 : begin					// JB,JNB,JBC
               case (instr_cycle)
                 2 : begin
                       case (cycle)
                         `c1 : pc_inc  <= biu_instr;
                         `c2 : begin
                                 auto_inc_pc <= 0;
                                 if ( ((act_instr[5] == 1) &&
                                       (bit_status != act_instr[4])) ||
                                       // (JB,JNB)
                                      ((act_instr[5] == 0) &&
                                       (bit_status == 1)))
                                       // (JBC)
                                 begin 
                                   if (act_instr[5] == 0)
                                   begin 
                                     //  JBC, clear only if bit set
                                     dest  <= t_dest;
                                   end 
                                 end 
				 sfr_rd = 1;	     // Fix for STAR 52000
                               end
                         `c3 : begin
                                 if ( ((act_instr[5] == 1) &&
                                       (bit_status != act_instr[4])) ||
                                       // (JB,JNB)
                                      ((act_instr[5] == 0) &&
                                       (bit_status == 1)))
                                 begin 
                                   result   <= new_pc;
                                   set_pc_n <= 0;
                                 end 
				 sfr_rd = 0;	      // Fix for STAR 52000
				 temp1 <= sfr_data_in;// Fix for STAR 52000
                               end
                         `c4 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        22 : begin					// CJNE
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c2 : begin
                                 sfr_addr = biu_instr;	// any case
                                 if ((act_instr[3:0] == 4'b0101) &&
				     (biu_instr[7] == 1))
				 begin
				     //compare with direct address
				     // in SFR area (CJNE A, direct,rel)
                                   sfr_rd   = 1;
				 end
				 ram_rd_n <= 0;		//in any way
                               end
                         `c3 : begin
                                 if (act_instr[3:0] == 4'b0101)
                                   temp2 <= sfr_data_in;
                                 else
                                 begin
                                   temp2 <= biu_instr;
                                 end 
				 ram_rd_n <= 1;
                                 sfr_rd  = 0;
                                 alu_op <= `alu_op_cmp;
                               end
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c1 : pc_inc <= biu_instr;	// rel address
                         `c2 : auto_inc_pc <= 0;
                         `c3 : begin
                                 if (alu_equal == 0)
                                 begin 
                                   result   <= new_pc;
                                   set_pc_n <= 0;
                                 end 
                                 chg_flags <= 1;
                               end
                         `c4 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        23 : begin					// LCALL
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         // increment sp before writing to @sp:
                         `c2 : begin
                                 sp_cnt_dir <= 1;		// up
                                 cnt_sp     <= 1;
                                 pc_inc     <= 8'b00000010;	// 2
                               end
                         `c3 : begin
                                 cnt_sp <= 0;
                                 temp1  <= new_pc[7:0];
                               end
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c2 : begin
                                 dest   <= 12;			// @sp
                                 pc_inc <= 8'b00000001;		// 1
                               end
                         `c3 : result[15:8] <= biu_instr;
                         `c4 : temp1        <= new_pc[15:8];
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c1 : begin
                                 cnt_sp <= 1;
                                 pc_inc <= 8'b00000001;		// 1
                               end
                         `c2 : begin
                                 cnt_sp <= 0;
                                 dest   <= 12;			// @sp
                                 auto_inc_pc <= 0;
                               end
                         `c3 : begin
                                 result[7:0] <= biu_instr;
                                 set_pc_n    <= 0;
                               end
                         `c4 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        24 : begin					// LJMP
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c2 : auto_inc_pc  <= 0;
                         `c3 : result[15:8] <= biu_instr;
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c3 : begin
                                 result[7:0] <= biu_instr;
                                 set_pc_n    <= 0;
                               end
                         `c4 : set_pc_n  <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        25 : begin					// RET,RETI
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : begin
                                 auto_inc_pc <= 0;	// stay
                                 src         <= 12;
                                 src_cycle   <= 1;
                               end
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c3 : begin
                                 result[15:8] <= alu;
                                 sp_cnt_dir   <= 0;	// down
                                 cnt_sp       <= 1;
                               end
                         `c4 : begin
                                 cnt_sp    <= 0;
                                 src_cycle <= 2;
                               end
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c3 : begin
                                 result[7:0] <= alu;
                                 set_pc_n    <= 0;
                                 cnt_sp      <= 1;
                                 if (act_instr[4] == 1)
                                   // RETI
                                   int_clr <= 1;
                               end
                         `c4 : begin
                                 set_pc_n <= 1;
                                 cnt_sp   <= 0;
                                 int_clr  <= 0;
                               end
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        26 : begin					// DJNZ direct,rel
               case (instr_cycle)
                 1 : begin
                       case (cycle)
                         `c2 : dest <= t_dest;
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c1 : pc_inc <= biu_instr;	// rel address
                         `c2 : auto_inc_pc <= 0;
                         `c3 : begin
                                 if (alu_zero == 0)
                                 begin 
                                   result   <= new_pc;
                                   set_pc_n <= 0;
                                 end 
                               end
                         `c4 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        27 : begin					// Interrupts
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : begin
                                 auto_inc_pc <= 0;
                                 int_rec     <= 0;
                                 sp_cnt_dir  <= 1;	// up
                                 cnt_sp      <= 1;
                               end
                         `c3 : begin
                                 cnt_sp <= 0;
                                 temp1  <= pc[7:0];
                               end
                         default: begin end
                       endcase
                     end
                 1 : begin
                       case (cycle)
                         `c2 : dest  <= 12;		// @sp
                         `c4 : temp1 <= pc[15:8];
                         default: begin end
                       endcase
                     end
                 2 : begin
                       case (cycle)
                         `c1 : cnt_sp <= 1;
                         `c2 : begin
                                 cnt_sp   <= 0;
                                 dest     <= 12;
                                 result   <= {8'b00000000,
                                              1'b0, int_src_rec, 3'b011};
                                 set_pc_n <= 0;
                               end
                         `c3 : set_pc_n <= 1;
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end

     // five cycle instructions:
        28 : begin					// MUL AB
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : begin
                                 auto_inc_pc <= 0;
                                 // 1st:
                                 temp1 <= b;
                                 temp2 <= 'b0;
                                 temp3 <= acc;
                               end
                         `c3,
                         `c4 : begin
                                 // 2nd/3rd:
                                 temp2 <= alu;
                                 temp3 <= alu_l;
                               end
                         default: begin end
                       endcase
                     end
                 1 : begin
                       // 4th/5th/6th/7th:
                       temp2 <= alu;
                       temp3 <= alu_l;
                     end
                 2 : begin
                       case (cycle )
                         `c1 : begin
                                 // 8th
                                 temp2 <= alu;
                                 temp3 <= alu_l;
                               end
                         // write results:
                         `c3 : begin
                                 sfr_addr = `b_addr;
                               end
                         `c4 : begin
                                 sfr_data_out = alu;
                                 sfr_wr       = 1;
                                 chg_flags   <= 1;
                                 acc_data    <= alu_l;
                                 ld_acc      <= 1;
                               end
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        29 : begin					// DIV AB
               case (instr_cycle)
                 0 : begin
                       case (cycle)
                         `c2 : begin
                                 auto_inc_pc <= 0;
                                 // initial:
                                 temp1 <= 'b0;
                                 temp2 <= 'b0;
                                 temp3 <= acc;
                               end
                         `c3 : begin
                                 // 1st:
                                 temp1 <= b;		// divisor
                                 temp2 <= alu;
                                 // correct false lower bit from first shift:
                                 temp3 <= {alu_l[7:1], 1'b0};
                               end
                         `c4 : begin
                                 // 2nd:
                                 temp2 <= alu;
                                 temp3 <= alu_l;
                               end
                         default: begin end
                       endcase
                     end
                 1 : begin
                       // 3rd/4th/5th/6th:
                       temp2 <= alu;
                       temp3 <= alu_l;
                     end
                 2 : begin
                       case (cycle)
                         `c1,
                         `c2 : begin
                                 // 7th/8th:
                                 temp2 <= alu;
                                 temp3 <= alu_l;
                               end
                         `c3 : begin
                                 sfr_addr  = `b_addr;
                                 acc_data <= alu_l;
                                 // shift right remainder:
                                 alu_op   <= `alu_op_div_sr;
                                 temp2    <= alu;
                               end
                         `c4 : begin
                                 sfr_data_out = alu;
                                 sfr_wr       = 1;
                                 chg_flags   <= 1;
                                 ld_acc      <= 1;
                               end
                         default: begin end
                       endcase
                     end
                 default: begin end
               endcase
             end
        default: begin end
      endcase


      // assignments:
      ram_addr         <= sfr_addr;
      ram_data_in      <= sfr_data_out;
      t_sfr_addr       <= sfr_addr;
      int_sfr_data_out <= sfr_data_out;
      int_sfr_wr       <= sfr_wr;
      ext_sfr_addr     <= sfr_addr;
      ext_sfr_data_out <= sfr_data_out;
      ext_sfr_wr       <= sfr_wr;
      ext_sfr_rd       <= sfr_rd;
    end 
  end 


  // assignments:
  assign  sfr_bit_addr    = {bit_nr[7:3], 3'b000};

  // assignments for output signals:
  assign  cpu_stop_mode_n = stop_mode_n;
  assign  cpu_idle_mode_n = (idle_mode_n & idle_mode_del_n);
  assign  cpu_bit_nr      = bit_nr;
  assign  cpu_temp1       = temp1;
  assign  cpu_temp2       = temp2;
  assign  cpu_temp3       = temp3;
  assign  int_sfr_addr    = t_sfr_addr;
  assign  int_ack         = t_int_ack;

endmodule
