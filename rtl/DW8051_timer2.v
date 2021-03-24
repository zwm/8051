// $Id: DW8051_timer2.v,v 1.1 1996/07/25 17:43:38 gina Exp $
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
// FILE: DW8051_timer2.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 timer2 module (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        10.06.96        Verilog version created
//
//	Bala Needamangalam
//			May 20,1998	Updated the inline comments
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_timer2 (clk,
                      rst_n,

                      // sfr bus:
                      sfr_addr,
                      timer2_sfr_cs,
                      timer2_data_out,
                      timer2_data_in,
                      sfr_wr,

                      // signals from DW8051_cpu:
                      cycle,
                      t2m,		//timer2 rate control bit from ckcon

                      // external inputs:
                      t2,		// input pin
                      t2ex,		// input pin

                      // external outputs:
                      t2_out,		// output pin

                      // signals to DW8051_serial:
                      out_rclk,		// receive  clock select from t2con
                      out_tclk,		// transmit clock select from t2con
                      out_t2_ofl,	// timer2 overflow flag

                      // signals to DW8051_intr:
                      out_tf2,		// overflow flag from t2con
                      out_exf2  	// external flag from t2con
		      );
 
 input clk;
 input rst_n;
 input [7:0] sfr_addr;
 input [7:0] timer2_data_in;
 input sfr_wr;
 input [1:0] cycle;
 input t2m;
 input t2;
 input t2ex;
 output timer2_sfr_cs;
 output [7:0] timer2_data_out;
 output t2_out;
 output out_rclk;
 output out_tclk;
 output out_t2_ofl;
 output out_tf2;
 output out_exf2;

//------------------------------------------------------------------------------
wire clk;
wire rst_n;
wire [7:0] sfr_addr;
wire [7:0] timer2_data_in;
wire sfr_wr;
wire [1:0] cycle;
wire t2m;
wire t2;
wire t2ex;
wire timer2_sfr_cs;
wire [7:0] timer2_data_out;
reg  t2_out;
wire out_rclk;
wire out_tclk;
wire out_t2_ofl;
wire out_tf2;
wire out_exf2;


//---------------
// local signals:
//---------------

// timer2_mode types:
`define auto_reload  2'b00
`define capture      2'b01
`define baud_rate    2'b10

reg  [7:0] t2con_reg;
wire tf2;
wire exf2;
wire rclk;
wire tclk;
wire exen2;
wire tr2;
wire ct2;
wire cp2;
 
wire [7:0] tl2_reg;
wire [7:0] th2_reg;
reg  [7:0] rcap2l_reg;
reg  [7:0] rcap2h_reg;
 
wire t2con_cs;			// chip select for t2con
wire tl2_cs;			// chip select for tl2
wire th2_cs;			// chip select for th2
wire rcap2l_cs;			// chip select for rcap2l
wire rcap2h_cs;			// chip select for rcap2h
 
wire [1:0] mode;		// of type timer2_mode
 
wire set_tf2;
wire e_tl2;			// enable low byte counter
wire e_th2;			// enable high byte counter
 
wire ld_tl2_n;			// active low load signal for low byte counter
wire ld_th2_n;			// active low load signal for high byte counter
 
wire [7:0] tl2_in;
wire [7:0] th2_in;
 
reg  t2_l0;			// latched input t2
reg  t2_l1;			// latched t2_l0
reg  t2_l2;			// latched t2_l1
 
reg  t2ex_l0;			// latched input t2ex
reg  t2ex_l1;			// latched t2ex_l0
reg  t2ex_l2;			// latched t2ex_l1
 
wire t2_event;			// 1-0 transition on t2 input
wire t2ex_event;		// 1-0 transition on t2ex input
wire tl2_sat;			// saturation flag for tl2_reg
wire th2_sat;			// saturation flag for th2_reg
wire tl2_wr;			// sfr write to tl2_reg
wire th2_wr;			// sfr write to th2_reg
wire t2_ofl;			// timer2 overflow flag
 
wire clk_div2;			// 1/2 clk
wire clk_div4;			// 1/4 clk
reg  clk_div12;			// 1/12 clk
reg  [1:0] count;
 

// dummy signals:
wire [7:0] tl2_reg_n;
wire [7:0] th2_reg_n;
wire tl2_sat5;
wire th2_sat5;

//------------------------------------------------------------------------------

  // decode sfr address
  assign t2con_cs  = (sfr_addr == `t2con_addr)  ? 1 : 0;
  assign tl2_cs    = (sfr_addr == `tl2_addr)    ? 1 : 0;
  assign th2_cs    = (sfr_addr == `th2_addr)    ? 1 : 0;
  assign rcap2l_cs = (sfr_addr == `rcap2l_addr) ? 1 : 0;
  assign rcap2h_cs = (sfr_addr == `rcap2h_addr) ? 1 : 0;

// SFR Write process
  always @(posedge clk or negedge rst_n)
  begin : sfr_process
    if (rst_n == 0)
    begin 
      t2con_reg  <= 'b0;
      rcap2l_reg <= 'b0;
      rcap2h_reg <= 'b0;
    end
    else
    begin
    		// t2con_reg can be updated by software or by hardware.
      if ((sfr_wr == 1) & (t2con_cs == 1)) t2con_reg <= timer2_data_in;
      else
      begin
        if (set_tf2    == 1) t2con_reg[7] <= 1;
        if (t2ex_event == 1) t2con_reg[6] <= 1;		//set exf2 flag
      end 

		// rcap2l_reg can be updated by software or by hardware in
		// capture mode.
      if ((sfr_wr == 1) & (rcap2l_cs == 1)) rcap2l_reg <= timer2_data_in;
      else if ((mode == `capture) &
               (t2ex_event == 1))           rcap2l_reg <= tl2_reg;

		// rcap2h_reg can be updated by software or by hardware in
		// capture mode.
      if ((sfr_wr == 1) & (rcap2h_cs == 1)) rcap2h_reg <= timer2_data_in;
      else if ((mode == `capture) &
               (t2ex_event == 1))           rcap2h_reg <= th2_reg;
    end 
  end  //sfr_process


  // Aliases for t2con bits
  assign  tf2   = t2con_reg[7];
  assign  exf2  = t2con_reg[6];
  assign  rclk  = t2con_reg[5];
  assign  tclk  = t2con_reg[4];
  assign  exen2 = t2con_reg[3];
  assign  tr2   = t2con_reg[2];
  assign  ct2   = t2con_reg[1];
  assign  cp2   = t2con_reg[0];


  // input flip-flops
  always @(posedge clk or negedge rst_n)
  begin : inp_latch_process
    if (rst_n == 0)
    begin 
      t2_l0   <= 0;
      t2_l1   <= 0;
      t2_l2   <= 0;
      t2ex_l0 <= 0;
      t2ex_l1 <= 0;
      t2ex_l2 <= 0;
    end
    else
    begin
      t2_l0   <= t2;		// Stage 0 synchronizer operates
      t2ex_l0 <= t2ex;		// on every clock.
      if (cycle == `c1)
      begin 
        t2_l1   <= t2_l0;	// Stage 1 and Stage 2 synchronizers are
        t2_l2   <= t2_l1;	// enabled only at the end of phase C1
        t2ex_l1 <= t2ex_l0;
        t2ex_l2 <= t2ex_l1;
      end 
    end 
  end  //inp_latch_process


  // detect high to low transition on t2 input
  assign t2_event  = (cycle != `c2) ? 0 : (t2_l2 & ~t2_l1);


  // detect high to low transition on t2ex input and evaluate enable flag
  assign t2ex_event  = (cycle != `c2) ? 0 :
                       (exen2 == 0)   ? 0 :
                                        (t2ex_l2  & ~t2ex_l1);


// Divide by 2 clock
  assign clk_div2 = cycle[0];

// Divide-by-4 clock: high only in phase C2
  assign clk_div4 = (cycle == `c2) ? 1 : 0;

// Divide by 12 clock process
// 		The "count" and the clk_div12 change state only at the end of
//		the C1 clock phase (i.e., in the beginning of C2)
  always @(posedge clk or negedge rst_n)
  begin : clk_div_process
    if (rst_n == 0)
    begin 
      count     <= 2'b00;
      clk_div12 <= 1;
    end
    else
    begin
      if (tr2 == 0)
      begin 
        count     <= 2'b00;
        clk_div12 <= 1;
      end
      else if (cycle == 2'b00)
      begin 
        count[0]  <= ~(count[0] | count[1]);
        count[1]  <= count[0];
        clk_div12 <= count[1];
      end
      else clk_div12 <= 0;
    end 
  end  //clk_div_process



// Timer2 Mode definitions.
  assign mode  = ((rclk == 1) | (tclk == 1)) ? `baud_rate :
                 (cp2 == 1)                  ? `capture   :
                                               `auto_reload;

// Write-enables for TL2/TH2
  assign  tl2_wr  = sfr_wr & tl2_cs;
  assign  th2_wr  = sfr_wr & th2_cs;

// Parallel load control for TL2
  assign ld_tl2_n = (tl2_wr == 1)                                ? 0  :
                    ((mode == `auto_reload) & (t2ex_event == 1)) ? 0  :
                    ((mode == `auto_reload) & (t2_ofl == 1))     ? 0  :
                    ((mode == `baud_rate)   & (t2_ofl == 1))     ? 0  :
                                                                   1;
// Parallel DATA in for TL2
  assign tl2_in  = (tl2_wr == 1) ? timer2_data_in : rcap2l_reg;

// Count-enable for TL2
  assign e_tl2  = (tr2  == 0)          ? 0         :
                  (ct2  == 1)          ? t2_event  :
                  (mode == `baud_rate) ? clk_div2  :
                  (t2m  == 0)          ? clk_div12 :
                                         clk_div4;

// Parallel load control for TH2
  assign ld_th2_n = (th2_wr == 1)                                ? 0  :
                    ((mode == `auto_reload) & (t2ex_event == 1)) ? 0  :
                    ((mode == `auto_reload) & (t2_ofl == 1))     ? 0  :
                    ((mode == `baud_rate)   & (t2_ofl == 1))     ? 0  :
                                                                   1;
// Parallel DATA in for TH2
  assign th2_in  = (th2_wr == 1) ? timer2_data_in : rcap2h_reg;

// Count-enable for TH2
  assign  e_th2  = tl2_sat;

// Timer2 Overflow definition
  assign  t2_ofl = th2_sat & tl2_sat;

// Hardware update flag for TF2 in T2CON
  assign set_tf2 = (mode != `baud_rate) ? t2_ofl : 0;

// The count-enables of TL2/TH2 are active only in the C2 clock phase.
// Thus the Timer/Ctr modules will update at the end of the C2 clock phase.
// This enables the CPU to read these registers in the C3 phase of the same
// machine cycle.

  DW8051_timer_ctr #(8) i1			// Timer/Ctr for TL2
                     (.q         (tl2_reg),
                      .q_n       (tl2_reg_n),   //not used
                      .ones_all  (tl2_sat),
                      .ones_5lsb (tl2_sat5),	//not used
                      .clk       (clk),
                      .rst_n     (rst_n),
                      .ld_n      (ld_tl2_n),
                      .data_in   (tl2_in),
                      .cnt_en    (e_tl2));
  DW8051_timer_ctr #(8) i2			// Timer/Ctr for TH2
                     (.q         (th2_reg),
                      .q_n       (th2_reg_n),   //not used
                      .ones_all  (th2_sat),
                      .ones_5lsb (th2_sat5),	//not used
                      .clk       (clk),
                      .rst_n     (rst_n),
                      .ld_n      (ld_th2_n),
                      .data_in   (th2_in),
                      .cnt_en    (e_th2));

  // output signals
  assign  out_tf2    = tf2  | set_tf2;
  assign  out_exf2   = exf2 | t2ex_event;
  assign  out_rclk   = rclk;
  assign  out_tclk   = tclk;
  assign  out_t2_ofl = t2_ofl;


  // t2_out is a 1 clock delayed version of t2_ofl
  always @(posedge clk or negedge rst_n)
  begin : out_process
    if (rst_n == 0)
    begin 
      t2_out <= 0;
    end
    else
      t2_out <= t2_ofl;
  end  //out_process


  assign timer2_data_out = (t2con_cs  == 1) ? t2con_reg  :
                           (tl2_cs    == 1) ? tl2_reg    :
                           (th2_cs    == 1) ? th2_reg    :
                           (rcap2l_cs == 1) ? rcap2l_reg :
                                              rcap2h_reg;

  assign  timer2_sfr_cs  = t2con_cs | tl2_cs | th2_cs | rcap2l_cs | rcap2h_cs;


endmodule
