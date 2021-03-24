// $Id: DW8051_intr_0.v,v 1.3 1996/07/30 14:33:40 uhlander Exp $
//------------------------------------------------------------------------------
//
//        This confidential and proprietary software may be used only
//     as authorized by a licensing agreement from synopsys Inc.
//     In the event of publication, the following notice is applicable:
//
//                    (C) COPYRIGHT 1996   SYNOPSYS INC.
//                          ALL RIGHTS RESERVED
//
//        The entire notice above must be reproduced on all authorized
//        copies.
//
// FILE: DW8051_intr_0.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 interrupt module for 6 interrupt sources (standard)
//           (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        04.06.96        Verilog version created
//	L.Rieder	17.07.96	IP returns 11xxxxxxb now.
//	G.Uhlaender	29.07.96	missing assignment to port smod1 added
//	L.Rieder	30.07.96	IP returns 10xxxxxxb now.
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//      Bala Needamangalam
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------



`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_intr_0 (clk,
		      rst_n,

                      // sfr bus signals:
                      sfr_addr,
                      intr_sfr_cs,
                      intr_data_out,
                      intr_data_in,
                      sfr_wr,
    
                      // signals from/to DW8051_cpu:
                      cycle,
                      int_req,
                      int_src,
                      int_ack,
                      int_clr,

                      // interrupt source signals:
                      int0_n,		// ext int 0
                      int1_n,		// ext int 1
                      tf0_set,		// timer 0 int
                      tf1_set,		// timer 1 int
                      ri0,		// receive  int
                      ti0,		// transmit int
                      tf2,		// timer 2 int
                      exf2,		// ext timer 2 int

                      // signals to timer module
                      ena_t0,
                      ena_t1,

                      // signal to serial port 1
                      smod1
		      );

 input clk;
 input rst_n;
 input [7:0]  sfr_addr;
 input [7:0]  intr_data_in;
 input sfr_wr;
 input [1:0]  cycle;
 input int_ack;
 input int_clr;
 input int0_n;
 input int1_n;
 input tf0_set;
 input tf1_set;
 input ri0;
 input ti0;
 input tf2;
 input exf2;
 output intr_sfr_cs;
 output [7:0]  intr_data_out;
 output  int_req;
 output [2:0]  int_src;
 output ena_t0;
 output ena_t1;
 output smod1;

//------------------------------------------------------------------------------
wire clk;
wire rst_n;
wire [7:0] sfr_addr;
wire [7:0] intr_data_in;
wire sfr_wr;
wire [1:0] cycle;
wire int_ack;
wire int_clr;
wire int0_n;
wire int1_n;
wire tf0_set;
wire tf1_set;
wire ri0;
wire ti0;
wire tf2;
wire exf2;
wire intr_sfr_cs;
wire [7:0] intr_data_out;
wire int_req;
wire [2:0] int_src;
wire ena_t0;
wire ena_t1;
wire smod1;

//---------------
// local signals:
//---------------
reg  [7:0] ie_reg;		// ie register
wire ea;
wire et0;
wire et1;
wire et2;
wire es0;
wire ex0;
wire ex1;
 
reg  [5:0] ip_reg;		// ip register
wire pt0;
wire pt1;
wire pt2;
wire ps0;
wire px0;
wire px1;
 
wire [7:0] tcon_reg;		// tcon register
reg  tf1;
reg  tr1;
reg  tf0;
reg  tr0;
reg  ie1;
reg  it1;
reg  ie0;
reg  it0;
 
reg  [7:0] eicon_reg;
wire eicon_cs;
 
wire ie_cs;			// chip select for ie-register
wire ip_cs;			// chip select for ip-register
wire tcon_cs;			// chip select for tcon-register
wire tcon_wr;
 
wire tf0_clr;
wire tf1_clr;
wire ie0_clr;
wire ie1_clr;
wire ie0_set;
wire ie1_set;
 
wire it0_set;
wire it1_set;
 
wire it0_clr;
wire it1_clr;
 
reg  ack_l;			// latched int_ack
 
reg  x0_l1_n;			// latched external interrupt 0
reg  x1_l1_n;			// latched external interrupt 1
reg  x0_l2_n;			// latched x0_l1_n
reg  x1_l2_n;			// latched x1_l1_n
reg  x0_l3_n;			// latched x0_l2_n
reg  x1_l3_n;			// latched x1_l2_n
 
wire x0_req;			// request from int0
wire x1_req;			// request from int1
wire t0_req;			// request from timer0
wire t1_req;			// request from timer1
wire t2_req;			// request from timer2
wire s0_req;			// request from serial port 0
 
wire x0_hp_req;			// high priority request from int0
wire x1_hp_req;			// high priority request from int1
wire t0_hp_req;			// high priority request from timer0
wire t1_hp_req;			// high priority request from timer1
wire t2_hp_req;			// high priority request from timer2
wire s0_hp_req;			// high priority request from serial port0
 
wire x0_lp_req;			// low priority request from int0
wire x1_lp_req;			// low priority request from int1
wire t0_lp_req;			// low priority request from timer0
wire t1_lp_req;			// low priority request from timer1
wire t2_lp_req;			// low priority request from timer2
wire s0_lp_req;			// low priority request from serial port0
 
wire [2:0] lp_src;		// source for low  priority requ.
wire [2:0] hp_src;		// source for high priority requ.
wire [2:0] i_src;		// source for active request
reg  [2:0] i_src_l;		// latched i_src
 
reg  iip0;			// low  priority interrupt in progress flag
reg  iip1;			// high priority interrupt in progress flag
wire iip0_set;
wire iip1_set;
wire iip0_clr;
wire iip1_clr;
 
wire hp_req;			// high priority request
wire lp_req;			// low  priority request
reg  hp_req_l;			// latched high priority request
reg  lp_req_l;			// latched low  priority request
 
 
// define interrupt source := interrupt_vector(5 downto 3)
`define x0_src_0 3'b000		// vector: 03h
`define x1_src_0 3'b010		// vector: 13h
`define t0_src_0 3'b001		// vector: 0Bh
`define t1_src_0 3'b011		// vector: 1Bh
`define t2_src_0 3'b101		// vector: 2Bh
`define s0_src_0 3'b100		// vector: 23h
 
//------------------------------------------------------------------------------

  // SFR Address decode
  assign ie_cs    = (sfr_addr == `ie_addr)    ? 1 : 0;
  assign ip_cs    = (sfr_addr == `ip_addr)    ? 1 : 0;
  assign tcon_cs  = (sfr_addr == `tcon_addr)  ? 1 : 0;
  assign eicon_cs = (sfr_addr == `eicon_addr) ? 1 : 0;

  // Defining the write-enable to TCON
  assign tcon_wr  = (tcon_cs & sfr_wr);

  // Software programming flags for set and reset of IT0
  assign it0_set  = (tcon_wr &  intr_data_in[0]);
  assign it0_clr  = (tcon_wr & ~intr_data_in[0]);

  // Software programming flags for set and reset of IT1
  assign it1_set  = (tcon_wr &  intr_data_in[2]);
  assign it1_clr  = (tcon_wr & ~intr_data_in[2]);

  assign ie0_set  = (cycle != `c1) ? 0                    :
                    (it0_clr == 1) ?  ~x0_l2_n            : // low-level sens.
                    (it0_set == 1) ? (~x0_l2_n & x0_l3_n) : // fall-edge sens.
                    (it0     == 0) ?  ~x0_l2_n            : // low-level sens.
                                     (~x0_l2_n & x0_l3_n);  // fall-edge sens.

  assign ie1_set  = (cycle != `c1) ? 0                    :
                    (it1_clr == 1) ?  ~x1_l2_n            : // low-level sens.
                    (it1_set == 1) ? (~x1_l2_n & x1_l3_n) : // fall-edge sens.
                    (it1     == 0) ?  ~x1_l2_n            : // low-level-sens.
                                     (~x1_l2_n & x1_l3_n);  // fall-edge sens.

  assign ie0_clr  = (((ack_l & it0) == 1) & (i_src_l == `x0_src_0)) ? 1 :
  						// int. ack cycle
                    (cycle != `c1) ? 0       :
                    (it0_set == 1) ? 0       :	// fall-edge sens & detected.
                    (it0_clr == 1) ? x0_l2_n :	// low-level sens & 1 detected
                    (it0     == 0) ? x0_l2_n :	// low-level sens & 1 detected
                                     0;

  assign ie1_clr  = (((ack_l & it1) == 1) & (i_src_l == `x1_src_0)) ? 1 :
  						// int. ack cycle
                    (cycle != `c1) ? 0       :
                    (it1_set == 1) ?      0  :	// fall-edge sens & detected
                    (it1_clr == 1) ? x1_l2_n :	// low-level sens & 1 detected
                    (it1     == 0) ? x1_l2_n :	// low-level sens & 1 detected
                                     0;

  assign tf0_clr  = ((ack_l == 1) & (i_src_l == `t0_src_0)) ? 1 : 0;
  			// TF0 flag is cleared in the int-ack cycle.

  assign tf1_clr  = ((ack_l == 1) & (i_src_l == `t1_src_0)) ? 1 : 0;
  			// TF1 flag is cleared in the int-ack cycle.


	// SFR Write process
  always @(posedge clk or negedge rst_n)
  begin : sfr_process
    if (rst_n == 0)
    begin 
      ie_reg <= 'b0;
      ip_reg <= 6'b000000;
      it0    <= 0;
      it1    <= 0;
      tr0    <= 0;
      tr1    <= 0;
      tf0    <= 0;
      tf1    <= 0;
      ie0    <= 0;
      ie1    <= 0;
      eicon_reg[7] <= 0;
    end
    else
    begin
      if (sfr_wr == 1)		// Software Update of registers
      begin 
        if (ie_cs == 1) ie_reg <= intr_data_in;
        if (ip_cs == 1) ip_reg <= intr_data_in[5:0];
        if (tcon_cs == 1)
        begin 
          it0 <= intr_data_in[0];
          it1 <= intr_data_in[2];
          tr0 <= intr_data_in[4];
          tr1 <= intr_data_in[6];
        end 
        if (eicon_cs == 1) eicon_reg[7] <= intr_data_in[7];
      end 

	// Hardware update of TF0 has greater priority than Software update.
           if (tf0_set == 1) tf0 <= 1;
      else if (tf0_clr == 1) tf0 <= 0;
      else if (tcon_wr == 1) tf0 <= intr_data_in[5];

	// Hardware update of TF1 has greater priority than Software update.
           if (tf1_set == 1) tf1 <= 1;
      else if (tf1_clr == 1) tf1 <= 0;
      else if (tcon_wr == 1) tf1 <= intr_data_in[7];

	// Hardware update of IE0 has greater priority than Software update.
           if (ie0_set == 1) ie0 <= 1;
      else if (ie0_clr == 1) ie0 <= 0;
      else if (tcon_wr == 1) ie0 <= intr_data_in[1];

	// Hardware update of IE1 has greater priority than Software update.
           if (ie1_set == 1) ie1 <= 1;
      else if (ie1_clr == 1) ie1 <= 0;
      else if (tcon_wr == 1) ie1 <= intr_data_in[3];
    end 
  end 

	// Aliases for IE register bits
  assign  ea  = ie_reg[7];
  assign  es0 = ie_reg[4];
  assign  et0 = ie_reg[1];
  assign  et1 = ie_reg[3];
  assign  et2 = ie_reg[5];
  assign  ex0 = ie_reg[0];
  assign  ex1 = ie_reg[2];

	// Aliases for IP register bits
  assign  ps0 = ip_reg[4];
  assign  pt0 = ip_reg[1];
  assign  pt1 = ip_reg[3];
  assign  pt2 = ip_reg[5];
  assign  px0 = ip_reg[0];
  assign  px1 = ip_reg[2];

	// Aliases for TCON register bits
  assign  tcon_reg[0] = it0;
  assign  tcon_reg[1] = ie0;
  assign  tcon_reg[2] = it1;
  assign  tcon_reg[3] = ie1;
  assign  tcon_reg[4] = tr0;
  assign  tcon_reg[5] = tf0;
  assign  tcon_reg[6] = tr1;
  assign  tcon_reg[7] = tf1;


  // external interrupt requests
  always @(posedge clk or negedge rst_n)
  begin : int_latch_process
    if (rst_n == 0)
    begin 
      x0_l1_n <= 0;
      x1_l1_n <= 0;
      x0_l2_n <= 0;
      x1_l2_n <= 0;
      x0_l3_n <= 0;
      x1_l3_n <= 0;
    end
    else
    begin
      x0_l1_n <= int0_n;	// Stage 1 synchronizers for int0_n and int1_n
      x1_l1_n <= int1_n;	// work at every clock
      if (cycle == `c4)
      begin 
        x0_l2_n <= x0_l1_n;	// Stage 2 and stage 3 synchronizers are
        x1_l2_n <= x1_l1_n;	// enabled only in clock phase C1 (i.e., end
        x0_l3_n <= x0_l2_n;	// of C4).
        x1_l3_n <= x1_l2_n;
      end 
    end 
  end 


  // Interrupt-Mask Logic
  assign  x0_req  = ie0 & ex0;
  assign  x1_req  = ie1 & ex1;
  assign  t0_req  = (tf0 | tf0_set) & et0;
  assign  t1_req  = (tf1 | tf1_set) & et1;
  assign  s0_req  = (ri0 | ti0)     & es0;
  assign  t2_req  = (tf2 | exf2)    & et2;

  // priorities of requests
  assign  x0_hp_req  = x0_req &  px0;
  assign  x0_lp_req  = x0_req & ~px0;
  assign  x1_hp_req  = x1_req &  px1;
  assign  x1_lp_req  = x1_req & ~px1;
  assign  t0_hp_req  = t0_req &  pt0;
  assign  t0_lp_req  = t0_req & ~pt0;
  assign  t1_hp_req  = t1_req &  pt1;
  assign  t1_lp_req  = t1_req & ~pt1;
  assign  t2_hp_req  = t2_req &  pt2;
  assign  t2_lp_req  = t2_req & ~pt2;
  assign  s0_hp_req  = s0_req &  ps0;
  assign  s0_lp_req  = s0_req & ~ps0;

  // interrupt sources
  
  // This is the high-priority queue
  assign hp_src  = (x0_hp_req == 1) ? `x0_src_0 :
                   (t0_hp_req == 1) ? `t0_src_0 :
                   (x1_hp_req == 1) ? `x1_src_0 :
                   (t1_hp_req == 1) ? `t1_src_0 :
                   (s0_hp_req == 1) ? `s0_src_0 :
                                      `t2_src_0;
				      
  // This is the low-priority queue 
  assign lp_src  = (x0_lp_req == 1) ? `x0_src_0 :
                   (t0_lp_req == 1) ? `t0_src_0 :
                   (x1_lp_req == 1) ? `x1_src_0 :
                   (t1_lp_req == 1) ? `t1_src_0 :
                   (s0_lp_req == 1) ? `s0_src_0 :
                                      `t2_src_0;

	// High priority interrupt request generation
  assign  hp_req  = (x0_hp_req |
                     x1_hp_req |
                     t0_hp_req |
                     t1_hp_req |
                     t2_hp_req |
                     s0_hp_req) & ea;

	// Low priority interrupt request generation
	// Note that this request is generated only when
	// there is no high pri interrupt yet, and when 
	// there isn't any highj-prio interrupt in progress 
	// (i.e., being serviced)
  assign  lp_req  = (x0_lp_req |
                     x1_lp_req |
                     t0_lp_req |
                     t1_lp_req |
                     t2_lp_req |
                     s0_lp_req) & ea & ~hp_req & ~iip1;


  // requests for all interrupts sampled at the begin of c3
  always @(posedge clk or negedge rst_n)
  begin : req_process
    if (rst_n == 0)
    begin 
      hp_req_l <= 0;
      lp_req_l <= 0;
    end
    else
    begin
      if (cycle == `c2)
      begin 
        hp_req_l <= hp_req;
        lp_req_l <= lp_req;
      end 
    end 
  end 


  // control signals for iip process
  assign  iip1_set = hp_req_l & int_ack;
  assign  iip1_clr = int_clr;
  assign  iip0_set = lp_req_l & int_ack;
  assign  iip0_clr = int_clr  & ~iip1;



  always @(posedge clk or negedge rst_n)
  begin : iip_process
    if  (rst_n == 0)
    begin 
      iip0 <= 0;
      iip1 <= 0;
    end
    else
    begin
           if (iip0_set == 1) iip0 <= 1;
      else if (iip0_clr == 1) iip0 <= 0;
           if (iip1_set == 1) iip1 <= 1;
      else if (iip1_clr == 1) iip1 <= 0;
    end 
  end 


  // compute output signals
  assign i_src       = (hp_req == 1) ? hp_src : lp_src;
  assign int_src     = i_src_l;
  assign int_req     = (hp_req_l & ~iip1) | (lp_req_l & ~iip0);
  assign intr_sfr_cs = ie_cs | ip_cs | tcon_cs | eicon_cs;

  // output mux
  assign intr_data_out = (ie_cs   == 1) ? ie_reg          :
                         (ip_cs   == 1) ? {2'b10, ip_reg} :
                         (tcon_cs == 1) ? tcon_reg        :
                                          {eicon_reg[7],7'b0000000};

  // output signals
  assign  ena_t0 = tr0;
  assign  ena_t1 = tr1;
  assign  smod1  = eicon_reg[7];


  // store source of actual interrupt
  always @(posedge clk or negedge rst_n)
  begin : i_src_latch_process
    if (rst_n == 0)
    begin 
      i_src_l <= 'b0;
    end
    else
    begin
      if (cycle == `c2) i_src_l <= i_src;
    end 
  end 


  // delay int_ack 1 cycle
  always @(posedge clk or negedge rst_n)
  begin : i_int_ack_process
    if (rst_n == 0)
    begin 
      ack_l <= 0;
    end
    else
    begin
      ack_l <= int_ack;
    end 
  end 


endmodule
