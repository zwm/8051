// $Id: DW8051_cpu.v,v 1.1 1996/07/25 17:42:42 gina Exp $
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
// FILE: DW8051_cpu.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 main cpu module (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        28.05.96        Verilog version created
//	L.Rieder	17.07.96	upper RAM returns FF if not existing
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needmanglam
//			May 20,98	Changed all GTECH instantiations to HDL.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_cpu (clk,
		   por_n,
                   rst_in_n,
                   rst_out_n,
                   test_mode_n,
                   cycle,
                   stop_mode_n,
                   idle_mode_n,

                   // sfr bus:
                   int_sfr_addr,
                   int_sfr_data_out,
                   int_sfr_data_in,
                   int_sfr_wr,
                   int_sfr_cs,
                   ext_sfr_addr,
                   ext_sfr_data_out,
    
                   ext_sfr_data_in,
                   ext_sfr_wr,
                   ext_sfr_rd,

                   // ext. memory interface
                   mem_addr,
                   mem_data_out,
                   mem_data_in,
                   mem_wr_n,
                   mem_rd_n,
                   mem_pswr_n,
                   mem_psrd_n,
                   mem_ale,
                   mem_ea_n,

                   // port control lines:
                   port_pin_reg_n,
                   p0_mem_reg_n,
                   p0_addr_data_n,
                   p2_mem_reg_n,
    
		   // Internal RAM interface (128 or 256 bytes):
		   iram_addr,
		   iram_data_out,
		   iram_data_in,
		   iram_rd_n,
		   iram_we1_n,
		   iram_we2_n,

                   // signals to serial:
                   smod,

                   // signals to timer:
                   tm,

                   // signals from/to DW8051_intr:
                   int_req,
                   int_src,
                   int_ack,
                   int_clr,

                   // internal ROM interface:
                   int_rom_data_in,
                   int_rom_rd_n,
                   int_rom_cs_n
		   );

parameter ram_256 = 0;		// 0: 128 Byte,     1: 256 Byte
parameter rom_addr_size = 0;	// 0..16 (0..64kB)
parameter extd_intr = 1;	// 0: std interrupt unit ( 7 interrupts)
				// 1: extended intr unit (13 interrupts)
 input clk;
 input por_n;
 input rst_in_n;
 input test_mode_n;
 input [7:0] int_sfr_data_in;
 input int_sfr_cs;
 input [7:0] ext_sfr_data_in;
 input [7:0] mem_data_in;
 input mem_ea_n;
 input int_req;
 input [3:0] int_src;
 input [7:0] iram_data_out;
 input [7:0] int_rom_data_in;
 output rst_out_n;
 output [1:0] cycle;
 output stop_mode_n;
 output idle_mode_n;
 output [7:0] int_sfr_addr;
 output [7:0] int_sfr_data_out;
 output int_sfr_wr;
 output [7:0] ext_sfr_addr;
 output [7:0] ext_sfr_data_out;
 output ext_sfr_wr;
 output ext_sfr_rd;
 output [15:0] mem_addr;
 output [7:0]  mem_data_out;
 output mem_wr_n;
 output mem_rd_n;
 output mem_pswr_n;
 output mem_psrd_n;
 output mem_ale;
 output port_pin_reg_n;
 output p0_mem_reg_n;
 output p0_addr_data_n;
 output p2_mem_reg_n;
 output [7:0] iram_addr;
 output [7:0] iram_data_in;
 output iram_rd_n;
 output iram_we1_n;
 output iram_we2_n;
 output smod;
 output [2:0] tm;
 output int_ack;
 output int_clr;
 output int_rom_rd_n;
 output int_rom_cs_n;

//------------------------------------------------------------------------------
//                             DESCRIPTION
//------------------------------------------------------------------------------
//
// Interrupts:
//            |c1|c2|c3|c4|c1|c2|c3|c4| ... |c1|c2|c3|c4|
// int_req  __xxxxxxxxxxxxx------xxxxxx     xxxxxxxxxxxxx
// int_ack  _____________________---___     _____________
// int_clr  ___________________________     _------------_  (RETI)
//
//------------------------------------------------------------------------------
wire clk;
wire por_n;
wire rst_in_n;
wire test_mode_n;
wire [7:0] int_sfr_data_in;
wire int_sfr_cs;
wire [7:0] ext_sfr_data_in;
wire [7:0] mem_data_in;
wire mem_ea_n;
wire int_req;
wire [3:0] int_src;
wire [7:0] int_rom_data_in;
wire rst_out_n;
wire [1:0] cycle;
wire stop_mode_n;
wire idle_mode_n;
wire [7:0] int_sfr_addr;
wire [7:0] int_sfr_data_out;
wire int_sfr_wr;
wire [7:0] ext_sfr_addr;
wire [7:0] ext_sfr_data_out;
wire ext_sfr_wr;
wire ext_sfr_rd;
wire [15:0] mem_addr;
wire [7:0] mem_data_out;
wire mem_wr_n;
wire mem_rd_n;
wire mem_pswr_n;
wire mem_psrd_n;
wire mem_ale;
wire port_pin_reg_n;
wire p0_mem_reg_n;
wire p0_addr_data_n;
wire p2_mem_reg_n;
wire [7:0] iram_addr;
wire [7:0] iram_data_in;
wire iram_rd_n;
wire iram_we1_n;
wire iram_we2_n;
wire smod;
wire [2:0] tm;
wire int_ack;
wire int_clr;
wire int_rom_rd_n;
wire int_rom_cs_n;


//---------------
// local signals:
//---------------
wire clk_n;
wire rst_n;
reg  por_n_del1;
reg  por_n_del2;
wire sync_por_n;
reg  old_rst_in_n;
reg  rst_first;
reg  sync_rst_n;
 
wire t_idle_mode_n;
wire t_stop_mode_n;
 
wire [7:0] ram_addr;
wire ram128_wr_addr_val_n;
wire ram256_wr_addr_val_n;
 
wire [7:0] ram_data_in;
wire ram128_wr_n;
wire ram256_wr_n;
wire ram_rd_n;
wire [7:0] t_sfr_addr;
wire [7:0] t_sfr_data_out;
wire [7:0] t_sfr_data_in;
wire t_sfr_wr;
wire [7:0] cpu_data_in;
wire cpu_sfr_cs;
wire [7:0] indir_data_in;
 
wire int_rec;
wire eie_eip_check;
 
// op_decoder signals:
wire [4:0] dec_itype;		// 0..31
wire [2:0] dec_last_cycle;	// 0..7
wire [3:0] dec_src;		// 0..15
wire [1:0] dec_src_cycle;	// 0..3
wire [3:0] dec_dest;		// 0.15
wire [5:0] dec_alu_op;
wire dec_chg_flags;
wire dec_rmw;
 
// cycle counter signals:
wire [1:0] t_cycle;
wire [1:0] t_cycle_n;		// dummy
 
// register:
wire [7:0] alu;
wire [7:0] alu_l;
wire [7:0] temp1;
wire [7:0] temp2;
wire [7:0] temp3;
wire [7:0] sp;			// Stack pointer
wire [7:0] dps;			// Data pointer sel
wire [7:0] pcon;		// Power control
wire [7:0] ckcon;		// Clock control
wire [7:0] psw;			// Prog.Stat.Word
wire [7:0] acc;			// Accumulator
wire [7:0] acc_data;
wire ld_acc;
wire ld_acc_direct;
wire [7:0] b;			// B register
wire [7:0] spc_fnc;		// SPC_FNC register
wire [7:0] sfr_reg_data_out;
wire sfr_reg_cs;
wire [7:0] mpage;
 
// lower/upper ram interface signals:
wire ram128_cs_n;
wire [7:0] ram128_data_out;
wire ram256_cs_n;
 
// pc signals:
wire [15:0] pc;
wire pc_tercnt;			// dummy
wire [15:0] result;		// res of execution
wire pc_cnt_dir;		// pc count direction
wire inc_pc;			// pc count enable
wire set_pc_n;
wire [7:0] pc_inc;		// pc increment
wire [7:0] pc_inc_h;
wire [15:0] pc_add;
wire pc_add_signed;
 
// 16 bit adder signals:
wire [15:0] add16_a;
wire [15:0] add16_b;
wire [15:0] add16_sum;
wire add16_co;			// dummy
wire sel_pc_dptr_n;
 
// dptr signals:
wire [15:0] dptr;
wire [7:0] dp_inc;		// dp increment
wire [15:0] dp_add;
 
// sp signals:
wire sp_cnt_dir;
wire cnt_sp;
 
// alu signals:
wire alu_co;
wire alu_aco;
wire alu_ovo;
wire [5:0] alu_op;
wire alu_zero;
wire alu_equal;
wire chg_flags;
 
// signals for bit operations:
wire bit_status;
wire [7:0] bit_nr;
 
// biu interface signals:
wire biu_wrs;
wire [15:0] biu_rom_addr;
wire [7:0] biu_instr;
wire [15:0] biu_ram_addr;
wire biu_wr_ram_addr_h;
wire biu_wr_ram_addr_l;
wire [7:0] biu_data_out;
wire biu_wr_ram;
wire biu_rd_ram;
wire biu_rd_rom;
wire [7:0] biu_data_in;
wire biu_ram_access_rdy;
wire [2:0] biu_md;
 

wire zero;
//------------------------------------------------------------------------------

  assign zero = 0;

  // if extended interrupt unit is present,
  // check for accesses to EIE and EIP:
  assign eie_eip_check = (extd_intr == 1) ? 1 : 0;


  //--------------------
  // Bus Interface Unit:
  //--------------------
  DW8051_biu #(rom_addr_size) i_biu
             (.clk             (clk),
              .rst_n           (rst_n),
              .cycle           (t_cycle),
              .ea_n            (mem_ea_n),
              .stop_mode_n     (t_stop_mode_n),
              .idle_mode_n     (t_idle_mode_n),
              .md              (biu_md),
              .wrs             (biu_wrs),
              .rom_addr        (biu_rom_addr),
              .instr_reg       (biu_instr),
              .ram_addr        (biu_ram_addr),
              .wr_ram_addr_h   (biu_wr_ram_addr_h),
              .wr_ram_addr_l   (biu_wr_ram_addr_l),
              .data_out        (biu_data_out),
              .wr_ram          (biu_wr_ram),
              .rd_ram          (biu_rd_ram),
              .rd_rom          (biu_rd_rom),
              .data_in         (biu_data_in),
              .ram_access_rdy  (biu_ram_access_rdy),
              .mem_addr        (mem_addr),
              .mem_data_out    (mem_data_out),
              .mem_data_in     (mem_data_in),
              .mem_wr_n        (mem_wr_n),
              .mem_rd_n        (mem_rd_n),
              .mem_pswr_n      (mem_pswr_n),
              .mem_psrd_n      (mem_psrd_n),
              .mem_ale         (mem_ale),
              .p0_mem_reg_n    (p0_mem_reg_n),
              .p0_addr_data_n  (p0_addr_data_n),
              .p2_mem_reg_n    (p2_mem_reg_n),
              .int_rom_data_in (int_rom_data_in),
              .int_rom_rd_n    (int_rom_rd_n),
              .int_rom_cs_n    (int_rom_cs_n));


  //---------------
  // implement alu:
  //---------------
  DW8051_alu i_alu 
             (.clk     (clk),
              .a       (temp1),
              .b       (temp2),
              .c       (temp3),
              .ci      (psw[7]),
              .aci     (psw[6]),
              .ovi     (psw[2]),
              .res     (alu),
              .c_res   (alu_l),
              .co      (alu_co),
              .aco     (alu_aco),
              .ovo     (alu_ovo),
              .zero    (alu_zero),
              .equal   (alu_equal),
              .bit_sts (bit_status),
              .bit_pos (bit_nr[2:0]),
              .alu_op  (alu_op));


  // Internal RAM Interface signals: 'Just passing through!
  assign iram_addr = ram_addr;
  assign iram_data_in = ram_data_in;
  assign iram_rd_n = ram_rd_n;
  assign iram_we1_n = ram128_wr_addr_val_n & ram256_wr_addr_val_n;
  assign iram_we2_n = ram128_wr_n & ram256_wr_n;

  // generate lower-128-byte-RAM secific signals:
  assign  ram128_cs_n = ram_addr[7];
  // generate upper-128-byte-RAM secific signals:
  assign  ram256_cs_n     = ~ram_addr[7];


  //----------------------------
  // build pc (program counter):
  //----------------------------
  DW8051_updn_ctr #(16) i_pc
                     (.data   (result),
                      .up_dn  (pc_cnt_dir),
                      .load   (set_pc_n),
                      .cen    (inc_pc),
                      .clk    (clk),
                      .reset  (rst_n),
                      .count  (pc),
                      .tercnt (pc_tercnt));	// not used



  // mux for signed/unsigned addition:
  // upper 8 bits '0' if unsigned, else pc_inc(7):
  assign  pc_inc_h[0] = pc_inc[7] & pc_add_signed;
  assign  pc_inc_h[1] = pc_inc[7] & pc_add_signed;
  assign  pc_inc_h[2] = pc_inc[7] & pc_add_signed;
  assign  pc_inc_h[3] = pc_inc[7] & pc_add_signed;
  assign  pc_inc_h[4] = pc_inc[7] & pc_add_signed;
  assign  pc_inc_h[5] = pc_inc[7] & pc_add_signed;
  assign  pc_inc_h[6] = pc_inc[7] & pc_add_signed;
  assign  pc_inc_h[7] = pc_inc[7] & pc_add_signed;
  assign  pc_add  = {pc_inc_h, pc_inc[7:0]};
  assign  dp_add  = {8'b00000000, dp_inc};

  // mux for A and B:
  assign  add16_a = (sel_pc_dptr_n == 0) ? dptr : pc;
  assign  add16_b = (sel_pc_dptr_n == 0) ? dp_add : pc_add;

  //------------------------------------------------------
  // 16 bit adder: used as pc incrementer (increment=rel),
  // dptr incrementer (increment=1) or
  // pc/dptr adder (increment=acc)
  //------------------------------------------------------

  assign add16_sum = add16_a + add16_b ;

  //----------------------
  // Indirect Data-In MUX:
  //----------------------
  assign indir_data_in[0] = (ram_256 == 0) ? (iram_data_out[0] | ram_addr[7]) :
                                             (iram_data_out[0]);
  assign indir_data_in[1] = (ram_256 == 0) ? (iram_data_out[1] | ram_addr[7]) :
                                             (iram_data_out[1]);
  assign indir_data_in[2] = (ram_256 == 0) ? (iram_data_out[2] | ram_addr[7]) :
                                             (iram_data_out[2]);
  assign indir_data_in[3] = (ram_256 == 0) ? (iram_data_out[3] | ram_addr[7]) :
                                             (iram_data_out[3]);
  assign indir_data_in[4] = (ram_256 == 0) ? (iram_data_out[4] | ram_addr[7]) :
                                             (iram_data_out[4]);
  assign indir_data_in[5] = (ram_256 == 0) ? (iram_data_out[5] | ram_addr[7]) :
                                             (iram_data_out[5]);
  assign indir_data_in[6] = (ram_256 == 0) ? (iram_data_out[6] | ram_addr[7]) :
                                             (iram_data_out[6]);
  assign indir_data_in[7] = (ram_256 == 0) ? (iram_data_out[7] | ram_addr[7]) :
                                             (iram_data_out[7]);
 
  //-----------------
  // sfr_data_in mux:
  //-----------------
  assign  cpu_sfr_cs  = int_sfr_cs | sfr_reg_cs | ram256_cs_n;
  assign t_sfr_data_in = (cpu_sfr_cs == 0) ? ext_sfr_data_in : cpu_data_in;

  // (test_mode_n(1): deselect RAM for scan test
  assign cpu_data_in = 
            ((ram128_cs_n == 0) && (test_mode_n == 1)) ? iram_data_out  : 
             (sfr_reg_cs == 1)                         ? sfr_reg_data_out :
                                                         int_sfr_data_in;
  // (test_mode_n(1): deselect RAM for scan test


  //-------------------------------------------------------
  // build cycle counter:
  // The cycle counter counts from 0 to 11 (12 states) for
  // the compatibility mode of the timer modules. All other
  // modules use only the lower 2 bits (4 states).
  //-------------------------------------------------------
  DW8051_u_ctr_clr #(2) cyc1
                     (.q     (t_cycle),
                      .q_n   (t_cycle_n),
                      .clk   (clk),
                      .clr_n (t_stop_mode_n),	// cleared in stop_mode
                      .rst_n (sync_por_n));	// reset only by por_n !


  //--------------------------------------
  // build decoder for actual instruction:
  //--------------------------------------
  DW8051_op_decoder i_opdec
                    (.op          (biu_instr),	// from DW8051_biu
                     .int         (int_rec),	//recognized at end of instr.
                     .idle_mode_n (t_idle_mode_n),
                     .itype       (dec_itype),
                     .last_cycle  (dec_last_cycle),
                     .src         (dec_src),
                     .src_cycle   (dec_src_cycle),
                     .dest        (dec_dest),
                     .alu_op      (dec_alu_op),
                     .chg_flags   (dec_chg_flags),
                     .rmw         (dec_rmw));


  //--------------------------
  // build main SFR registers:
  //--------------------------
  DW8051_main_regs i_mregs
                   (.clk              (clk),
                    .rst_n            (rst_n),
                    .sfr_addr         (t_sfr_addr),
                    .sfr_reg_cs       (sfr_reg_cs),
                    .sfr_reg_data_out (sfr_reg_data_out),
                    .sfr_data_out     (t_sfr_data_out),
                    .sfr_wr           (t_sfr_wr),

                    .biu_data_in      (biu_data_in),
                    .acc_data         (acc_data),

                    .cycle            (t_cycle),
                    .sp_cnt_dir       (sp_cnt_dir),
                    .cnt_sp           (cnt_sp),
                    .ld_acc           (ld_acc),
                    .ld_acc_direct    (ld_acc_direct),
                    .chg_flags        (chg_flags),
                    .alu_co           (alu_co),
                    .alu_aco          (alu_aco),
                    .alu_ovo          (alu_ovo),

                    .sp               (sp),
                    .dptr             (dptr),
                    .dps              (dps),
                    .pcon             (pcon),
                    .ckcon            (ckcon),
                    .psw              (psw),
                    .acc              (acc),
                    .b                (b),
                    .spc_fnc          (spc_fnc),
                    .mpage            (mpage));


  //---------------------
  // main control module:
  //---------------------
  DW8051_control i_control
                 (.clk                  (clk),
                  .rst_n                (rst_n),
                  .cycle                (t_cycle),

                  // inputs:
                  .biu_instr            (biu_instr),
                  .biu_ram_access_rdy   (biu_ram_access_rdy),

                  .int_req              (int_req),
                  .int_src              (int_src),

                  .alu                  (alu),
                  .alu_l                (alu_l),
                  .sp                   (sp),
                  .dps                  (dps),
                  .pcon                 (pcon),
                  .psw                  (psw),
                  .acc                  (acc),
                  .b                    (b),
                  .dptr                 (dptr),
                  .indir_data_in        (indir_data_in),
                  .pc                   (pc),
                  .add16_sum            (add16_sum),
                  .eie_eip_check        (eie_eip_check),
                  .mpage                (mpage),

                  .dec_itype            (dec_itype),
                  .dec_last_cycle       (dec_last_cycle),
                  .dec_src              (dec_src),
                  .dec_src_cycle        (dec_src_cycle),
                  .dec_dest             (dec_dest),
                  .dec_alu_op           (dec_alu_op),
                  .dec_chg_flags        (dec_chg_flags),
                  .dec_rmw              (dec_rmw),

                  .alu_zero             (alu_zero),
                  .alu_equal            (alu_equal),
                  .bit_status           (bit_status),

                  // outputs:
                  .cpu_idle_mode_n      (t_idle_mode_n),
                  .cpu_stop_mode_n      (t_stop_mode_n),

                  .biu_ram_addr         (biu_ram_addr),
                  .biu_wr_ram_addr_h    (biu_wr_ram_addr_h),
                  .biu_wr_ram_addr_l    (biu_wr_ram_addr_l),
                  .biu_data_out         (biu_data_out),
                  .biu_wr_ram           (biu_wr_ram),
                  .biu_rd_ram           (biu_rd_ram),
                  .biu_rd_rom           (biu_rd_rom),

                  .ram_addr             (ram_addr),
                  .ram128_wr_addr_val_n (ram128_wr_addr_val_n),
                  .ram256_wr_addr_val_n (ram256_wr_addr_val_n),

                  .ram_data_in          (ram_data_in),
                  .ram128_wr_n          (ram128_wr_n),
                  .ram256_wr_n          (ram256_wr_n),
                  .ram_rd_n             (ram_rd_n),
                  .int_sfr_addr         (t_sfr_addr),
                  .int_sfr_data_out     (t_sfr_data_out),
                  .int_sfr_wr           (t_sfr_wr),
                  .ext_sfr_addr         (ext_sfr_addr),
                  .ext_sfr_data_out     (ext_sfr_data_out),
                  .ext_sfr_wr           (ext_sfr_wr),
                  .ext_sfr_rd           (ext_sfr_rd),
                  .sfr_data_in          (t_sfr_data_in),
                  .port_pin_reg_n       (port_pin_reg_n),

                  .int_ack              (int_ack),
                  .int_clr              (int_clr),
                  .int_rec              (int_rec),

                  .cpu_temp1            (temp1),
                  .cpu_temp2            (temp2),
                  .cpu_temp3            (temp3),
                  .result               (result),

                  .pc_cnt_dir           (pc_cnt_dir),
                  .inc_pc               (inc_pc),
                  .set_pc_n             (set_pc_n),
                  .pc_inc               (pc_inc),
                  .dp_inc               (dp_inc),
                  .pc_add_signed        (pc_add_signed),
                  .sel_pc_dptr_n        (sel_pc_dptr_n),

                  .sp_cnt_dir           (sp_cnt_dir),
                  .cnt_sp               (cnt_sp),

                  .alu_op               (alu_op),
                  .chg_flags            (chg_flags),
                  .acc_data             (acc_data),
                  .ld_acc               (ld_acc),
                  .ld_acc_direct        (ld_acc_direct),

                  .cpu_bit_nr           (bit_nr));

  //---------------------------------------------------------
  // 2 stage synchronizer for por_n:
  // The release of por_n is synchronized to the rising
  // edge of clk since the load on the reset net is
  // normally big enougth to meet recovery time requirements.
  //---------------------------------------------------------
  always @(posedge clk)
  begin : por_del_proc
    por_n_del1  <= por_n;
    por_n_del2  <= por_n_del1;
   end 


  //-------------------------------------------------------
  // normal rst_in_n sample process:
  // rst_in_n state is sampled at the end of c4. rst_in_n
  // must be asserted at least for 2 samples in order to be
  // recognized.
  //-------------------------------------------------------
  always @(posedge clk)
  begin : rst_proc
    if (sync_por_n == 0)
    begin 
      rst_first    <= 0;		// in first cycle
      old_rst_in_n <= rst_in_n;	// save current state
      sync_rst_n   <= rst_in_n;	// follow rst_in_n state
    end
    else
    begin
      // check rst_in state in c4:
      if (t_cycle == `c4)
      begin 
        if (((old_rst_in_n == 1) && (rst_in_n == 0)) |
            ((old_rst_in_n == 0) && (rst_in_n == 1)))
        begin 
          // rst_in goes low or high:
          rst_first <= 1;
        end 

        // action only in second cycle:
        if (rst_first == 1)
        begin 
          if (rst_in_n == 0)		// still applied ?
          begin 
            sync_rst_n <= 0;
            rst_first  <= 0;
          end
          else if (rst_in_n == 1)
          begin 
            sync_rst_n <= 1;
            rst_first  <= 0;
          end 
        end 

        old_rst_in_n <= rst_in_n;	// save current state
      end 
    end 
  end 	// rst_proc


  //-----------------------------
  // internal signal assignments:
  //-----------------------------
  assign  sync_por_n   = (por_n  & (por_n_del2 | (~test_mode_n)));
                         // controllable
  assign  rst_n        = (sync_por_n  & (sync_rst_n | (~test_mode_n)));
                         // por_n always
  assign  biu_wrs      = spc_fnc[0];
  assign  biu_rom_addr = pc;
  assign  biu_md       = ckcon[2:0];

  //--------------------------
  //output signal assignments:
  //--------------------------
  assign  rst_out_n        = rst_n;
  assign  cycle            = t_cycle;
  assign  stop_mode_n      = t_stop_mode_n;
  assign  idle_mode_n      = t_idle_mode_n;
  assign  int_sfr_addr     = t_sfr_addr;
  assign  int_sfr_data_out = t_sfr_data_out;
  assign  int_sfr_wr       = t_sfr_wr;
  assign  tm               = ckcon[5:3];
  assign  smod             = pcon[7];


endmodule

