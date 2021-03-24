// $Id: DW8051_main_regs.v,v 1.1 1996/07/25 17:43:04 gina Exp $
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
// FILE: DW8051_main_regs.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 main special function registers (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        31.05.96        Verilog version created
//      Bala Needamangalam
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------
 

`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_main_regs (// standard signals:
                         clk,
                         rst_n,

                         // sfr bus signals:
                         sfr_addr,
                         sfr_reg_cs,
                         sfr_reg_data_out,
                         sfr_data_out,
                         sfr_wr,
    
                         // accu data in:
                         biu_data_in,
                         acc_data,

                         // control signals:
                         cycle,
                         sp_cnt_dir,
                         cnt_sp,
                         ld_acc,
                         ld_acc_direct,
                         chg_flags,
                         alu_co,
                         alu_aco,
                         alu_ovo,

                         // sfr regs:
                         sp,		// Stack pointer
                         dptr,		// Data pointer
                         dps,		// Data pointer sel
                         pcon,		// Power control
                         ckcon,		// Clock control
                         psw,		// Prog.Stat.Word
                         acc,		// Accumulator
                         b,		// B register
                         spc_fnc,	// SPC_FNC register
                         mpage		// Memory page reg.
			 );
 input clk;
 input rst_n;
 input [7:0]  sfr_addr;
 input [7:0]  sfr_data_out;
 input sfr_wr;
 input [7:0]  biu_data_in;
 input [7:0]  acc_data;
 input [1:0]  cycle;
 input sp_cnt_dir;
 input cnt_sp;
 input ld_acc;
 input ld_acc_direct;
 input chg_flags;
 input alu_co;
 input alu_aco;
 input alu_ovo;
 output sfr_reg_cs;
 output [7:0]  sfr_reg_data_out;
 output [7:0]  sp;
 output [15:0] dptr;
 output [7:0]  dps;
 output [7:0]  pcon;
 output [7:0]  ckcon;
 output [7:0]  psw;
 output [7:0]  acc;
 output [7:0]  b;
 output [7:0]  spc_fnc;
 output [7:0]  mpage;

//------------------------------------------------------------------------------
wire clk;
wire rst_n;
wire [7:0] sfr_addr;
wire [7:0] sfr_data_out;
wire sfr_wr;
wire [7:0] biu_data_in;
wire [7:0] acc_data;
wire [1:0] cycle;
wire sp_cnt_dir;
wire cnt_sp;
wire ld_acc;
wire ld_acc_direct;
wire chg_flags;
wire alu_co;
wire alu_aco;
wire alu_ovo;
wire sfr_reg_cs;
wire [7:0] sfr_reg_data_out;
wire [7:0] sp;
wire [15:0] dptr;
wire [7:0] dps;
wire [7:0] pcon;
wire [7:0] ckcon;
wire [7:0] psw;
wire [7:0] acc;
wire [7:0] b;
wire [7:0] spc_fnc;
wire [7:0] mpage;

//---------------
// local signals:
//---------------
reg  [ 7:0] i_sp;
reg  [15:0] i_dp0;
reg  [15:0] i_dp1;
reg  i_dps;
reg  [ 5:0] i_pcon;
reg  [ 7:0] i_ckcon;
reg  [ 7:1] i_psw;
reg  [ 7:0] i_acc;
reg  [ 7:0] i_b;
reg  i_spc_fnc;
reg  [ 7:0] i_mpage;
 
wire [ 7:0] acc_data_in;
wire acc_pty;
wire acc_pty0;
wire acc_pty1;
wire acc_pty2;
wire acc_pty3;
wire acc_pty4;
wire acc_pty5;
 
wire cs_sp;
wire cs_dpl;
wire cs_dph;
wire cs_dpl1;
wire cs_dph1;
wire cs_dps;
wire cs_pcon;
wire cs_ckcon;
wire cs_psw;
wire cs_acc;
wire cs_b;
wire cs_spc_fnc;
wire cs_mpage;
 
//------------------------------------------------------------------------------


  //----------------------------------------------------
  // Build sfr registers accessible by all instructions.
  // These registers can generally be read and
  // written via the sfr bus.
  //----------------------------------------------------
  assign cs_sp      = (sfr_addr == `sp_addr)      ? 1  : 0;
  assign cs_dpl     = (sfr_addr == `dpl_addr)     ? 1  : 0;
  assign cs_dph     = (sfr_addr == `dph_addr)     ? 1  : 0;
  assign cs_dpl1    = (sfr_addr == `dpl1_addr)    ? 1  : 0;
  assign cs_dph1    = (sfr_addr == `dph1_addr)    ? 1  : 0;
  assign cs_dps     = (sfr_addr == `dps_addr)     ? 1  : 0;
  assign cs_pcon    = (sfr_addr == `pcon_addr)    ? 1  : 0;
  assign cs_ckcon   = (sfr_addr == `ckcon_addr)   ? 1  : 0;
  assign cs_psw     = (sfr_addr == `psw_addr)     ? 1  : 0;
  assign cs_acc     = (sfr_addr == `acc_addr)     ? 1  : 0;
  assign cs_b       = (sfr_addr == `b_addr)       ? 1  : 0;
  assign cs_spc_fnc = (sfr_addr == `spc_fnc_addr) ? 1  : 0;
  assign cs_mpage   = (sfr_addr == `mpage_addr)   ? 1  : 0;


  always @(posedge clk or negedge rst_n)
  begin : sfr_reg_file
    if (rst_n == 0)
    begin 
      i_sp      <= 8'b00000111;
      i_dp0     <= 'b0;
      i_dp1     <= 'b0;
      i_dps     <= 0;
      i_pcon    <= 6'b000000;
      i_ckcon   <= 8'b00000001;
      i_psw     <= 'b0;
      i_acc     <= 'b0;
      i_b       <= 'b0;
      i_spc_fnc <= 0;
      i_mpage   <= 'b0;
    end
    else
    begin
      if (sfr_wr == 1)
      begin 
             if (cs_sp      == 1) i_sp        <= sfr_data_out;
        else if (cs_dpl     == 1) i_dp0[7:0]  <= sfr_data_out;
        else if (cs_dph     == 1) i_dp0[15:8] <= sfr_data_out;
        else if (cs_dpl1    == 1) i_dp1[7:0]  <= sfr_data_out;
        else if (cs_dph1    == 1) i_dp1[15:8] <= sfr_data_out;
        else if (cs_dps     == 1) i_dps       <= sfr_data_out[0];
        else if (cs_pcon    == 1) i_pcon      <= {sfr_data_out[7:6],
                                                  sfr_data_out[3:0]};
        else if (cs_ckcon   == 1) i_ckcon     <= sfr_data_out;
        else if (cs_psw     == 1) i_psw       <= sfr_data_out[7:1];
        else if (cs_acc     == 1) i_acc       <= sfr_data_out;
        else if (cs_b       == 1) i_b         <= sfr_data_out;
        else if (cs_spc_fnc == 1) i_spc_fnc   <= sfr_data_out[0];
        else if (cs_mpage   == 1) i_mpage     <= sfr_data_out;
      end 

      // for some instructions accu needs to be loaded direct:
      if ((ld_acc == 1) || (ld_acc_direct == 1))
      begin 
        i_acc <= acc_data_in;
      end 

      // update program status word (psw) if desired:
      if ((chg_flags == 1) && (cycle == `c1))
      begin 
        i_psw <= {alu_co, alu_aco, i_psw[5:3], alu_ovo, i_psw[1]};
      end 

      // count sp if desired:
      if (cnt_sp == 1)
      begin 
        if (sp_cnt_dir == 1)
          i_sp <= (i_sp + 1);	// up
        else
          i_sp <= (i_sp - 1);	// down
      end 
    end 
  end 	// sfr_reg_file


  // output mux:
  assign sfr_reg_data_out = (cs_sp      == 1) ? i_sp                   :
                            (cs_dpl     == 1) ? i_dp0[7:0]             :
                            (cs_dph     == 1) ? i_dp0[15:8]            :
                            (cs_dpl1    == 1) ? i_dp1[7:0]             :
                            (cs_dph1    == 1) ? i_dp1[15:8]            :
                            (cs_dps     == 1) ? {7'b0000000, i_dps}    :
                            (cs_pcon    == 1) ? {i_pcon[5:4], 2'b11,
                                                 i_pcon[3:0]}          :
                            (cs_ckcon   == 1) ? i_ckcon                :
                            (cs_psw     == 1) ? {i_psw[7:1], acc_pty}  :
                            (cs_acc     == 1) ? i_acc                  :
                            (cs_b       == 1) ? i_b                    :
                            (cs_spc_fnc == 1) ? {7'b0000000, i_spc_fnc} :
                                                i_mpage;

  // common cs:
  assign sfr_reg_cs = (cs_sp      | 
                       cs_dpl     | cs_dph  |
                       cs_dpl1    | cs_dph1 |
                       cs_dps     |
                       cs_pcon    |
                       cs_ckcon   |
                       cs_psw     |
                       cs_acc     |
                       cs_b       |
                       cs_spc_fnc |
                       cs_mpage);


  //------------------
  // Accu data_in mux:
  //------------------
  assign acc_data_in  = (ld_acc_direct == 0) ? acc_data : biu_data_in;


  //-----------------------
  // Build accu parity bit:
  // (written to psw along
  // with write to acc)
  //-----------------------
  assign acc_pty = i_acc[7] ^ i_acc[6] ^ i_acc[5] ^ i_acc[4] ^
		   i_acc[3] ^ i_acc[2] ^ i_acc[1] ^ i_acc[0];

  //--------------------------------
  // Assignments for output signals:
  //--------------------------------
  assign  sp      = i_sp;
  assign  dptr    = (i_dps == 0) ? i_dp0 : i_dp1;
  assign  dps     = {7'b0000000, i_dps};
  assign  pcon    = {i_pcon[5:4], 2'b11, i_pcon[3:0]};
  assign  ckcon   = i_ckcon;
  assign  psw     = {i_psw[7:1], acc_pty};
  assign  acc     = i_acc;
  assign  b       = i_b;
  assign  spc_fnc = {7'b0000000, i_spc_fnc};
  assign  mpage   = i_mpage;

endmodule
