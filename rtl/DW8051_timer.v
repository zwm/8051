// $Id: DW8051_timer.v,v 1.1 1996/07/25 17:43:34 gina Exp $
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
// FILE: DW8051_timer.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 timer module (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        05.06.96        Verilog version created
//	L.Rieder	22.07.96
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//			May 20,1998	Updated the inline comments
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_timer (clk,
                     rst_n,

                     // sfr bus:
                     sfr_addr,
                     timer_sfr_cs,
                     timer_data_out,
                     timer_data_in,
                     sfr_wr,

                     // external inputs:
                     t0,		// ext timer input 0
                     t1,		// ext timer input 1
                     int0_n,		// ext int 0
                     int1_n,		// ext int 1

                     // signals from DW8051_cpu:
                     cycle,
                     t0m,		// timer0 rate control bit from ckcon
                     t1m,		// timer1 rate control bit from ckcon

                     // signals from/to register TCON in DW8051_intr:
                     ena_t0,		// timer0 enable
                     ena_t1,		// timer1 enable
                     tf0_set,		// set timer0 interrupt flag in tcon
                     tf1_set,		// set timer1 interrupt flag in tcon

                     // baudrate output signal to DW8051_serial:
                     t1_ofl,		// timer1 overflow flag

                     // external outputs:
                     t0_out,		// timer0 overflow flag
                     t1_out  		// timer1 overflow flag
		     );
 
 input clk;
 input rst_n;
 input [7:0] sfr_addr;
 input [7:0] timer_data_in;
 input sfr_wr;
 input t0;
 input t1;
 input int0_n;
 input int1_n;
 input [1:0]  cycle;
 input t0m;
 input t1m;
 input ena_t0;
 input ena_t1;
 output timer_sfr_cs;
 output [7:0] timer_data_out;
 output tf0_set;
 output tf1_set;
 output t1_ofl;
 output t0_out;
 output t1_out;

//------------------------------------------------------------------------------
wire clk;
wire rst_n;
wire [7:0] sfr_addr;
wire [7:0] timer_data_in;
wire sfr_wr;
wire t0;
wire t1;
wire int0_n;
wire int1_n;
wire [1:0] cycle;
wire t0m;
wire t1m;
wire ena_t0;
wire ena_t1;
wire timer_sfr_cs;
wire [7:0] timer_data_out;
wire tf0_set;
wire tf1_set;
wire t1_ofl;
reg  t0_out;
reg  t1_out;


//---------------
// local signals:
//---------------

// timer0_mode types:
`define t0_auto_reload  2'b00
`define t0_13_bit       2'b01
`define t0_16_bit       2'b10
`define t0_2x8_bit      2'b11

// timer1_mode types:
`define t1_auto_reload  2'b00
`define t1_13_bit       2'b01
`define t1_16_bit       2'b10
`define t1_off          2'b11

wire [1:0] mode0;		// of type timer0_mode;
wire [1:0] mode1;      		// of type timer1_mode;
 
reg  [7:0] tmod_reg;
wire t1_gated_n;
wire ct1;
wire m10;
wire m11;
wire t0_gated_n;
wire ct0;
wire m00;
wire m01;
 
 
wire [7:0] th0_reg;
wire [7:0] th1_reg;
wire [7:0] tl0_reg;
wire [7:0] tl1_reg;
// tcon  register implemented in DW8051_intr unit
// ckcon register implemented in DW8051_???
 
wire tmod_cs;			// chip select for tmod register
wire th0_cs;			// chip select for th0 register
wire th1_cs;			// chip select for th1 register
wire tl0_cs;			// chip select for tl0 register
wire tl1_cs;			// chip select for tl0 register
 
wire clk_div4;			// 1/4  of clk
reg  clk_div12_t0;		// 1/12 of clk
reg  clk_div12_t1;		// 1/12 of clk
reg  clk_div12_t0_h;		// 1/12 of clk
 
reg  [1:0] count_t0;
reg  [1:0] count_t1;
reg  [1:0] count_t0_h;
 
wire e_th0;			// enable timer0 high byte counter
wire e_tl0;			// enable timer0 low  byte counter
wire e_th1;			// enable timer1 high byte counter
wire e_tl1;			// enable timer1 low  byte counter
 
wire ld_th0_n;			// load timer0 high byte
wire ld_tl0_n;			// load timer0 low  byte
wire ld_th1_n;			// load timer1 high byte
wire ld_tl1_n;			// load timer1 low  byte
 
wire [7:0] th0_in;
wire [7:0] tl0_in;
wire [7:0] th1_in;
wire [7:0] tl1_in;
 
wire tl0_wr;			// sfr write to tl0_reg
wire th0_wr;			// sfr write to th0_reg
wire tl1_wr;			// sfr write to tl1_reg
wire th1_wr;			// sfr write to th1_reg
 
reg  t0_l0;			// latched input t0
reg  t1_l0;			// latched input t1
reg  t0_l1;			// latched t0_l0
reg  t1_l1;			// latched t1_l0
reg  t0_l2;			// latched t0_l1
reg  t1_l2;			// latched t1_l1
 
reg  int0_l_n;			// latched input int0_n
reg  int1_l_n;			// latched input int1_n
 
reg  gate0;
reg  gate1;
 
wire t0_event;			// negative transition on t0 input
wire t1_event;			// negative transition on t1 input
 
wire tl0_sat5;			// saturation on 5  LSBs of tl0_reg
				// (i.e. tl0_reg = `XXX11111`)
wire tl0_sat8;			// saturation on 8 LSBs (=all bits) of
				// tl0_reg (i.e. tl0_reg='11111111'
wire th0_sat8;			// saturation on 8 LSBs (=all bits) of
				// th0_reg (i.e. th0_reg='11111111'
wire tl1_sat5;			// saturation on 5 LSBs of tl1_reg
				// (i.e. tl1_reg = `XXX11111`)
wire tl1_sat8;			// saturation on 8 LSBs (=all bits) of
				// tl1_reg  (i.e. tl1_reg='11111111'
wire th1_sat8;			// saturation on 8 LSBs (=all bits) of
				// th1_reg  (i.e. th1_reg='11111111'
 
wire t1_ofl_i;			// timer1 overflow (internal)
wire t0_ofl_i;			// timer0 overflow (internal)
 

//dummy signals:
wire [7:0] tl0_reg_n;
wire [7:0] tl1_reg_n;
wire [7:0] th0_reg_n;
wire [7:0] th1_reg_n;
wire th0_sat5;
wire th1_sat5;

//------------------------------------------------------------------------------


  // decode sfr address
  assign tmod_cs = (sfr_addr == `tmod_addr) ? 1 : 0;
  assign th0_cs  = (sfr_addr == `th0_addr)  ? 1 : 0;
  assign th1_cs  = (sfr_addr == `th1_addr)  ? 1 : 0;
  assign tl0_cs  = (sfr_addr == `tl0_addr)  ? 1 : 0;
  assign tl1_cs  = (sfr_addr == `tl1_addr)  ? 1 : 0;


  // SFR Write process
  always @(posedge clk or negedge rst_n)
  begin : sfr_process
    if (rst_n == 0)
    begin 
      tmod_reg  <= 'b0;
    end
    else
    begin
      if (sfr_wr == 1)
      begin 
        if (tmod_cs == 1) tmod_reg <= timer_data_in;
      end 
    end 
  end  //sfr_process


  // aliases for tmod bits
  assign  t1_gated_n  = ~tmod_reg[7];
  assign  ct1         =  tmod_reg[6];
  assign  m11         =  tmod_reg[5];
  assign  m10         =  tmod_reg[4];
  assign  t0_gated_n  = ~tmod_reg[3];
  assign  ct0         =  tmod_reg[2];
  assign  m01         =  tmod_reg[1];
  assign  m00         =  tmod_reg[0];


  // input flip-flops
  always @(posedge clk or negedge rst_n)
  begin : inp_latch_process
    if (rst_n == 0)
    begin 
      t0_l0    <= 0;
      t1_l0    <= 0;
      t0_l1    <= 0;
      t1_l1    <= 0;
      t0_l2    <= 0;
      t1_l2    <= 0;
      gate0    <= 0;
      gate1    <= 0;
      int0_l_n <= 0;
      int1_l_n <= 0;
    end
    else
    begin
      t0_l0    <= t0;		// Stage 0 synchronizer
      t1_l0    <= t1;		// works unconditionally for all clock phases
      int0_l_n <= int0_n;
      int1_l_n <= int1_n;
      if (cycle == `c1)
      begin 
        t0_l1 <= t0_l0;		// Stage 1 and Stage 2 synchronizers
        t1_l1 <= t1_l0;		// are enabled only at the end of
        t0_l2 <= t0_l1;		// clock phase C1
        t1_l2 <= t1_l1;
        gate0 <= int0_l_n;
        gate1 <= int1_l_n;
      end 
    end 
  end  //inp_latch_process


  // detect high to low transition on t0,t1 inputs
  assign t0_event = (cycle == `c2) ? (t0_l2 & ~t0_l1) : 0;
  assign t1_event = (cycle == `c2) ? (t1_l2 & ~t1_l1) : 0;

// Mode 0 definitions
  assign mode0 = ((m00 == 0) & (m01 == 0)) ? `t0_13_bit      :
                 ((m00 == 1) & (m01 == 0)) ? `t0_16_bit      :
                 ((m00 == 0) & (m01 == 1)) ? `t0_auto_reload :
                                             `t0_2x8_bit;
// Mode 1 definitions
  assign mode1 = ((m10 == 0) & (m11 == 0)) ? `t1_13_bit      :
                 ((m10 == 1) & (m11 == 0)) ? `t1_16_bit      :
                 ((m10 == 0) & (m11 == 1)) ? `t1_auto_reload :
                                             `t1_off;
  // Definining the write-enables for TH0/1 and TL0/1
  assign  tl0_wr = sfr_wr & tl0_cs;
  assign  th0_wr = sfr_wr & th0_cs;
  assign  tl1_wr = sfr_wr & tl1_cs;
  assign  th1_wr = sfr_wr & th1_cs;

// Parallel load control for TL0
  assign ld_tl0_n = (tl0_wr == 1)              ? 0         :
                    (mode0 == `t0_auto_reload) ? ~tl0_sat8 :
                                                 1;
// Parallel load control for TH0
  assign ld_th0_n = (th0_wr == 1) ? 0 : 1;

// Parallel load DATA for TL0
  assign tl0_in  = (tl0_wr == 1) ? timer_data_in : th0_reg;

// Parallel load DATA for TH0
  assign th0_in  = timer_data_in;

// Count enable for TL0
  assign e_tl0  = (ena_t0 == 0)               ? 0            :
                  ((t0_gated_n | gate0) == 0) ? 0            :
                  (ct0 == 1)                  ? t0_event     :
                  (t0m == 0)                  ? clk_div12_t0 :
                                              clk_div4;

// Count enable for TH0
  assign e_th0  = (mode0  == `t0_13_bit)      ? tl0_sat5       :
                  (mode0  == `t0_16_bit)      ? tl0_sat8       :
                  // mode0 = t0_2x8_bit
                  (mode0  == `t0_auto_reload) ? 0              :
                  (ena_t1 == 0)               ? 0              :
                  (t0m    == 0)               ? clk_div12_t0_h :
                                                clk_div4;
                  // (when t0m = 1)

// Parallel load control for TL1
  assign ld_tl1_n = (tl1_wr == 1)               ? 0         :
                    (mode1  == `t1_auto_reload) ? ~tl1_sat8 :
                                                  1;

// Parallel load control for TH1
  assign ld_th1_n = (th1_wr == 1) ? 0 : 1;

// Parallel load DATA for TL1
  assign tl1_in  = (tl1_wr == 1) ? timer_data_in : th1_reg;

// Parallel load DATA for TH1
  assign th1_in  = timer_data_in;

// Count enable for TL1
  assign e_tl1  = ((ena_t1 == 0) & (mode0 != `t0_2x8_bit)) ? 0            :
                  (mode1 == `t1_off)                       ? 0            :
                  ((t1_gated_n | gate1) == 0)              ? 0            :
                  (ct1   == 1)                             ? t1_event     :
                  (t1m   == 0)                             ? clk_div12_t1 :
                                                             clk_div4;
// Count enable for TH1
  assign e_th1  = (mode1 == `t1_13_bit) ? tl1_sat5 :
                  (mode1 == `t1_16_bit) ? tl1_sat8 :
                                          0;	// when mode1 = t1_auto_reload
                                                //   or mode1 = t1_off

// The count-enables for TH0/TL0 and TH1/TL1 are active in the C2 clock phase.
// Thus, all the timer/ctr modules will change states at the end of the C2
// clock phase.  This enables the CPU to read these registers in the C3 phase
// of the same machine cycle.

  DW8051_timer_ctr #(8) i1			// Timer/Ctr for TL0
                     (.q         (tl0_reg),
                      .q_n       (tl0_reg_n),	//not used
                      .ones_all  (tl0_sat8),
                      .ones_5lsb (tl0_sat5),
                      .clk       (clk),
                      .rst_n     (rst_n),
                      .ld_n      (ld_tl0_n),
                      .data_in   (tl0_in),
                      .cnt_en    (e_tl0));

  DW8051_timer_ctr #(8) i2			// Timer/Ctr for TH0
                     (.q         (th0_reg),
                      .q_n       (th0_reg_n),	//not used
                      .ones_all  (th0_sat8),
                      .ones_5lsb (th0_sat5),	// not used
                      .clk       (clk),
                      .rst_n     (rst_n),
                      .ld_n      (ld_th0_n),
                      .data_in   (th0_in),
                      .cnt_en    (e_th0));

  DW8051_timer_ctr #(8) i3			// Timer/Ctr for TL1
                     (.q         (tl1_reg),
                      .q_n       (tl1_reg_n),	//not used
                      .ones_all  (tl1_sat8),
                      .ones_5lsb (tl1_sat5),
                      .clk       (clk),
                      .rst_n     (rst_n),
                      .ld_n      (ld_tl1_n),
                      .data_in   (tl1_in),
                      .cnt_en    (e_tl1));

  DW8051_timer_ctr #(8) i4			// Timer/Ctr for TH1
                     (.q         (th1_reg),
                      .q_n       (th1_reg_n),	//not used
                      .ones_all  (th1_sat8),
                      .ones_5lsb (th1_sat5),	// not used
                      .clk       (clk),
                      .rst_n     (rst_n),
                      .ld_n      (ld_th1_n),
                      .data_in   (th1_in),
                      .cnt_en    (e_th1));


// Divide-by-4 clock is high only in cycle C2
  assign clk_div4  = (cycle == `c2) ? 1 : 0;

// Divide-by-12 clock process
// The "count", clk_div12_t0, clk_div12_t0_h and clk_div12_t1 are updated in the
// beginning of the C2 clock phase.
  always @(posedge clk or negedge rst_n)
  begin : clk_div_process
    if (rst_n == 0)
    begin 
      count_t0       <= 2'b00;
      count_t1       <= 2'b00;
      count_t0_h     <= 2'b00;
      clk_div12_t0   <= 1;
      clk_div12_t1   <= 1;
      clk_div12_t0_h <= 1;
    end
    else
    begin
      if (ena_t0 == 0)
      begin 
        count_t0     <= 2'b00;
        clk_div12_t0 <= 1;
      end
      else if (cycle == 2'b00)
      begin 
        count_t0[0]  <= ~(count_t0[0] | count_t0[1]);
        count_t0[1]  <= count_t0[0];
        clk_div12_t0 <= count_t0[1];
      end
      else clk_div12_t0 <= 0;


      if ((mode0 == `t0_2x8_bit) && (ena_t1 == 0))
      begin
        count_t0_h     <= 2'b00;
        clk_div12_t0_h <= 1;
      end
      else if (cycle == 2'b00)
      begin
        count_t0_h[0]  <= ~(count_t0_h[0] | count_t0_h[1]);
        count_t0_h[1]  <= count_t0_h[0];
        clk_div12_t0_h <= count_t0_h[1];
      end
      else clk_div12_t0_h <= 0;


      if (((mode0 != `t0_2x8_bit) && (ena_t1 == 0)) || (mode1 == `t1_off))
      begin 
        count_t1     <= 2'b00;
        clk_div12_t1 <= 1;
      end
      else if (cycle == 2'b00)
      begin 
        count_t1[0]  <= ~(count_t1[0] | count_t1[1]);
        count_t1[1]  <= count_t1[0];
        clk_div12_t1 <= count_t1[1];
      end
      else clk_div12_t1 <= 0;
    end 
  end  //clk_div_process


// Defining the Timer 1 overflow conditions
  assign t1_ofl_i = ((mode1 == `t1_13_bit) | (mode1 == `t1_16_bit)) ? th1_sat8 :
                    (mode1 == `t1_auto_reload)                      ? tl1_sat8 :
                                                                      0;
                     // when  mode1 = t1_off

// Defining the Timer 1 overflow conditions
  assign t0_ofl_i = ((mode0 == `t0_13_bit) | (mode0 == `t0_16_bit)) ? th0_sat8 :
                                                                      tl0_sat8;
                    // low byte overflow is output in auto-reload mode
                    // and in 2x8 bit mode!


  // output signals
  assign timer_data_out = (tmod_cs == 1) ? tmod_reg :
                          (th0_cs  == 1) ? th0_reg  :
                          (th1_cs  == 1) ? th1_reg  :
                          (tl0_cs  == 1) ? tl0_reg  :
                                           tl1_reg;

  assign  timer_sfr_cs  = tmod_cs | th0_cs | th1_cs | tl0_cs | tl1_cs;

  assign tf0_set  = (mode0 == `t0_13_bit) ? th0_sat8 :
                    (mode0 == `t0_16_bit) ? th0_sat8 :
                                            tl0_sat8;
                    // when mode0 = t0_2x8_bit
                    //   or mode0 = t0_auto_reload

  assign tf1_set = (mode0 == `t0_2x8_bit) ? th0_sat8 : t1_ofl_i;
  assign t1_ofl  = t1_ofl_i;


// t0_out and t1_out are 1 clock delayed versions of t0_ofl_i and t1_ofl_i
// respectively.
  always @(posedge clk or negedge rst_n)
   begin : out_process
    if (rst_n == 0)
    begin 
      t0_out <= 0;
      t1_out <= 0;
    end
    else
    begin
      t0_out <= t0_ofl_i;
      t1_out <= t1_ofl_i;
    end 
  end  //out_process

endmodule
