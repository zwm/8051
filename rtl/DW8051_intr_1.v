// $Id: DW8051_intr_1.v,v 1.1 1996/07/25 17:43:01 gina Exp $
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
// FILE: DW8051_intr_1.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 interrupt module for 13 interrupt sources (extended)
//           (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        28.05.96        Verilog version created
//
//      Bala Needamangalam
//                      Sep 15.98       Fix for STAR 58773: DW8051 Extended
//                                      Interrupt Unit malfuctions with Timer
//                                      0/1 interrupts and read-modify-write
//                                      instructions:
//                                      Changed the qualification for tf0_clr
//					and tf1_clr to "ack_l", instead of
//					"int_ack".
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------



`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"


module DW8051_intr_1 (clk,
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
                      int2,		// ext int 2
                      int3_n,		// ext int 3
                      int4,		// ext int 4
                      int5_n,		// ext int 5
                      tf0_set,		// timer 0 int
                      tf1_set,		// timer 1 int
                      ri0,		// serial port 0 receive  int
                      ti0,		// serial port 0 transmit int
                      ri1,		// serial port 1 receive  int
                      ti1,		// serial port 1 transmit int
                      tf2,		// timer 2 int
                      exf2,		// ext timer 2 int
                      pfi,		// power fail int
                      wdti,		// watchdog timeout int

                      // signals to timer module
                      ena_t0,
                      ena_t1,

                      // signal to serial port 1 module
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
 input int2;
 input int3_n;
 input int4;
 input int5_n;
 input tf0_set;
 input tf1_set;
 input ri0;
 input ti0;
 input ri1;
 input ti1;
 input tf2;
 input exf2;
 input pfi;
 input wdti;
 output intr_sfr_cs;
 output [7:0]  intr_data_out;
 output int_req;
 output [3:0]  int_src;
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
 wire int2;
 wire int3_n;
 wire int4;
 wire int5_n;
 wire tf0_set;
 wire tf1_set;
 wire ri0;
 wire ti0;
 wire ri1;
 wire ti1;
 wire tf2;
 wire exf2;
 wire pfi;
 wire wdti;
 wire intr_sfr_cs;
 wire [7:0] intr_data_out;
 wire int_req;
 wire [3:0] int_src;
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
wire es1;
wire ex0;
wire ex1;
 
reg  [6:0] ip_reg;		// ip register
wire pt0;
wire pt1;
wire pt2;
wire ps0;
wire ps1;
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
 
reg  [7:4] exif_reg;		// exif register
wire ie2;
wire ie3;
wire ie4;
wire ie5;
 
reg  [4:0] eie_reg;		// eie register
wire ewdi;
wire ex5;
wire ex4;
wire ex3;
wire ex2;
 
reg  [4:0] eip_reg;		// eip register
wire pwdi;
wire px5;
wire px4;
wire px3;
wire px2;
 
wire [7:0] eicon_reg;		// eicon register
 
reg  smod1_reg;
reg  epfi;
reg  pfi_reg;
reg  wdti_reg;
 
wire wdti_set;
wire pfi_set;
 
wire ie_cs;			// chip select for ie-register
wire ip_cs;			// chip select for ip-register
wire tcon_cs;			// chip select for tcon-register
wire exif_cs;			// chip_select for exif-register
wire eie_cs;			// chip_select for eie-register
wire eip_cs;			// chip_select for eip-register
wire eicon_cs;			// chip_select for eicon_register
				// (includes: smod1reg, epfi, pfi_reg and
				// wdti_reg)
wire eicon_wr;
wire tcon_wr;
 
wire tf0_clr;
wire tf1_clr;
wire ie0_clr;
wire ie1_clr;
 
wire ie0_set;
wire ie1_set;
wire ie2_set;
wire ie3_set;
wire ie4_set;
wire ie5_set;
 
wire it0_set;
wire it1_set;
 
wire it0_clr;
wire it1_clr;
 
reg  ack_l;			// latched int_ack
 
reg  x0_l1_n;			// latched external interrupt 0
reg  x1_l1_n;			// latched external interrupt 1
reg  x2_l1;			// latched external interrupt 2
reg  x3_l1_n;			// latched external interrupt 3
reg  x4_l1;			// latched external interrupt 4
reg  x5_l1_n;			// latched external interrupt 5
 
reg  x0_l2_n;			// latched x0_l1_n
reg  x1_l2_n;			// latched x1_l1_n
reg  x2_l2;			// latched x2_l1
reg  x3_l2_n;			// latched x3_l1_n
reg  x4_l2;			// latched x4_l1
reg  x5_l2_n;			// latched x5_l1_n
 
reg  x0_l3_n;			// latched x0_l2_n
reg  x1_l3_n;			// latched x1_l2_n
reg  x2_l3;			// latched x2_l2
reg  x3_l3_n;			// latched x3_l2_n
reg  x4_l3;			// latched x4_l2
reg  x5_l3_n;			// latched x5_l2_n
 
reg  pfi_l1;			// latched pfi
reg  pfi_l2;			// latched pfi_l1
 
reg  wdti_l1;			// latched wdti
reg  wdti_l2;			// latched wdti_l1
reg  wdti_l3;			// latched wdti_l2
 
wire x0_req;			// request from int0
wire x1_req;			// request from int1
wire x2_req;			// request from int2
wire x3_req;			// request from int3
wire x4_req;			// request from int4
wire x5_req;			// request from int5
wire t0_req;			// request from timer0
wire t1_req;			// request from timer1
wire t2_req;			// request from timer2
wire s0_req;			// request from serial port 0
wire s1_req;			// request from serial port 1
wire wd_req;			// request from watchdog timer
wire pf_req;			// request from power fail detector
 
wire x0_hp_req;			// high priority request from int0
wire x1_hp_req;			// high priority request from int1
wire x2_hp_req;			// high priority request from int2
wire x3_hp_req;			// high priority request from int3
wire x4_hp_req;			// high priority request from int4
wire x5_hp_req;			// high priority request from int5
wire t0_hp_req;			// high priority request from timer0
wire t1_hp_req;			// high priority request from timer1
wire t2_hp_req;			// high priority request from timer2
wire s0_hp_req;			// high priority request from serial port0
wire s1_hp_req;			// high priority request from serial port1
wire wd_hp_req;			// high priority request from watchdog timer
 
wire x0_lp_req;			// low priority request from int0
wire x1_lp_req;			// low priority request from int1
wire x2_lp_req;			// low priority request from int2
wire x3_lp_req;			// low priority request from int3
wire x4_lp_req;			// low priority request from int4
wire x5_lp_req;			// low priority request from int5
wire t0_lp_req;			// low priority request from timer0
wire t1_lp_req;			// low priority request from timer1
wire t2_lp_req;			// low priority request from timer2
wire s0_lp_req;			// low priority request from serial port0
wire s1_lp_req;			// low priority request from serial port1
wire wd_lp_req;			// low priority request from watchdog timer
 
wire [3:0] lp_src;		// source for low  priority requ.
wire [3:0] hp_src;		// source for high priority requ.
wire [3:0] i_src;		// source for active request
reg  [3:0] i_src_l;		// latched i_src
 
reg  iip0;			// low  priority interrupt in progress flag
reg  iip1;			// high priority interrupt in progress flag
reg  iip2;			// power fail interrupt in progress
wire iip0_set;
wire iip1_set;
wire iip2_set;
wire iip0_clr;
wire iip1_clr;
wire iip2_clr;
 
reg  pf_req_l;			// latched highest priority request
                                // (from power fail detector)
wire hp_req;			// high priority request
wire lp_req;			// low  priority request
reg  hp_req_l;			// latched high priority request
reg  lp_req_l;			// latched low  priority request
 
// define interrupt source := interrupt_vector(6 downto 3)
`define  x0_src 4'b0000		// vector: 03h
`define  x1_src 4'b0010		// vector: 13h
`define  t0_src 4'b0001		// vector: 0Bh
`define  t1_src 4'b0011		// vector: 1Bh
`define  t2_src 4'b0101		// vector: 2Bh
`define  s0_src 4'b0100		// vector: 23h
`define  s1_src 4'b0111		// vector: 3Bh
`define  pf_src 4'b0110		// vector: 33h
`define  x2_src 4'b1000		// vector: 43h
`define  x3_src 4'b1001		// vector: 4Bh
`define  x4_src 4'b1010		// vector: 53h
`define  x5_src 4'b1011		// vector: 5Bh
`define  wd_src 4'b1100		// vector: 63h
 

//------------------------------------------------------------------------------

  // SFR Address decode
  assign ie_cs    = (sfr_addr == `ie_addr)    ? 1 : 0;
  assign ip_cs    = (sfr_addr == `ip_addr)    ? 1 : 0;
  assign tcon_cs  = (sfr_addr == `tcon_addr)  ? 1 : 0;
  assign exif_cs  = (sfr_addr == `exif_addr)  ? 1 : 0;
  assign eie_cs   = (sfr_addr == `eie_addr)   ? 1 : 0;
  assign eip_cs   = (sfr_addr == `eip_addr)   ? 1 : 0;
  assign eicon_cs = (sfr_addr == `eicon_addr) ? 1 : 0;

  // Write-enables for EICON and TCON
  assign eicon_wr = sfr_wr & eicon_cs;
  assign tcon_wr  = sfr_wr & tcon_cs;

  // Software programming for IT0 set and clear
  assign it0_set  = tcon_wr &  intr_data_in[0];
  assign it0_clr  = tcon_wr & ~intr_data_in[0];

  // Software programming for IT1 set and clear
  assign it1_set  = tcon_wr &  intr_data_in[2];
  assign it1_clr  = tcon_wr & ~intr_data_in[2];

  // Defining the IE0 flag settings
  assign ie0_set  = (cycle != `c1) ? 0                    :
                    (it0_clr == 1) ?  ~x0_l2_n            : // Low-level sens.
                    (it0_set == 1) ? (~x0_l2_n & x0_l3_n) : // Fall-edge sens.
                    (it0     == 0) ?  ~x0_l2_n            : // Low-level sens.
                                     (~x0_l2_n & x0_l3_n);  // Fall-edge sens.
  // Defining the IE1 flag settings
  assign ie1_set  = (cycle != `c1) ? 0                    :
                    (it1_clr == 1) ?  ~x1_l2_n            : // Low-level sens.
                    (it1_set == 1) ? (~x1_l2_n & x1_l3_n) : // Fall-edge sens.
                    (it1     == 0) ?  ~x1_l2_n            : // Low-level sens.
                                     (~x1_l2_n & x1_l3_n);  // Fall-edge sens.

  // Defining the IE2 flag settings
  assign ie2_set  = (cycle != `c1) ? 0 : ( x2_l2   & ~x2_l3);   // rise edge

  // Defining the IE3 flag settings
  assign ie3_set  = (cycle != `c1) ? 0 : (~x3_l2_n &  x3_l3_n); // fall-edge

  // Defining the IE4 flag settings
  assign ie4_set  = (cycle != `c1) ? 0 : ( x4_l2   & ~x4_l3);   // rise edge

  // Defining the IE5 flag settings
  assign ie5_set  = (cycle != `c1) ? 0 : (~x5_l2_n &  x5_l3_n); // fall-edge

  assign ie0_clr  = (((ack_l & it0) == 1) & (i_src_l == `x0_src)) ? 1 :
  					// int-ack cycle
                    (cycle != `c1) ? 0       :
                    (it0_set == 1) ? 0       : // fall-edge sens & detected
                    (it0_clr == 1) ? x0_l2_n : // low-level sens & 1 detected
                    (it0     == 0) ? x0_l2_n : // low-level sens & 1 detected
                                     0;

  assign ie1_clr  = (((ack_l & it1) == 1) & (i_src_l == `x1_src)) ? 1 :
  					// int-ack cycle
                    (cycle != `c1) ? 0       :
                    (it1_set == 1) ? 0       : // fall-edge sens & detected
                    (it1_clr == 1) ? x1_l2_n : // low-level sens & 1 detected
                    (it1     == 0) ? x1_l2_n : // low-level sens & 1 detected
                                     0;

  assign tf0_clr  = ((ack_l == 1) & (i_src_l == `t0_src)) ? 1 : 0;
  		// TF0 flag is cleared in the int-ack cycle.

  assign tf1_clr  = ((ack_l == 1) & (i_src_l == `t1_src)) ? 1 : 0;
  		// TF1 flag is cleared in the int-ack cycle.

  assign pfi_set  = (cycle != `c1) ? 0 : pfi_l2;
  		// PFI is a level-sensitive interrupt.

  assign wdti_set = (cycle != `c1) ? 0 : (wdti_l2 & ~wdti_l3);
  		// WDTI is an edge-sensitive interrupt


	// SFR Write process
  always @(posedge clk or negedge rst_n)
  begin : sfr_process
    // cs_process
    if (rst_n == 0)
    begin 
      ie_reg    <= 'b0;
      ip_reg    <= 7'b0000000;
      it0       <= 0;
      it1       <= 0;
      tr0       <= 0;
      tr1       <= 0;
      tf0       <= 0;
      tf1       <= 0;
      ie0       <= 0;
      ie1       <= 0;
      exif_reg  <= 4'b0000;
      eie_reg   <= 5'b00000;
      eip_reg   <= 5'b00000;
      smod1_reg <= 0;
      epfi      <= 0;
      pfi_reg   <= 0;
      wdti_reg  <= 0;
    end
    else
    begin
      if (sfr_wr == 1)		// Software update of registers
      begin 
        if (ie_cs == 1) ie_reg <= intr_data_in;
        if (ip_cs == 1) ip_reg <= intr_data_in[6:0];
        if (tcon_cs == 1)
        begin 
          it0 <= intr_data_in[0];
          it1 <= intr_data_in[2];
          tr0 <= intr_data_in[4];
          tr1 <= intr_data_in[6];
        end 
        if (eie_cs == 1) eie_reg <= intr_data_in[4:0];
        if (eip_cs == 1) eip_reg <= intr_data_in[4:0];
      end 

	// Hardware update of TF0 has greater priority than software update.
           if (tf0_set == 1) tf0 <= 1;
      else if (tf0_clr == 1) tf0 <= 0;
      else if (tcon_wr == 1) tf0 <= intr_data_in[5];

	// Hardware update of TF1 has greater priority than software update.
           if (tf1_set == 1) tf1 <= 1;
      else if (tf1_clr == 1) tf1 <= 0;
      else if (tcon_wr == 1) tf1 <= intr_data_in[7];

	// Hardware update of IE0 has greater priority than software update.
           if (ie0_set == 1) ie0 <= 1;
      else if (ie0_clr == 1) ie0 <= 0;
      else if (tcon_wr == 1) ie0 <= intr_data_in[1];

	// Hardware update of IE0 has greater priority than software update.
           if (ie1_set == 1) ie1 <= 1;
      else if (ie1_clr == 1) ie1 <= 0;
      else if (tcon_wr == 1) ie1 <= intr_data_in[3];

	// Hardware update of IE2 has greater priority than software update.
           if (ie2_set == 1)                   exif_reg[4] <= 1;
      else if ((sfr_wr == 1) & (exif_cs == 1)) exif_reg[4] <= intr_data_in[4];

	// Hardware update of IE3 has greater priority than software update.
           if (ie3_set == 1)                   exif_reg[5] <= 1;
      else if ((sfr_wr == 1) & (exif_cs == 1)) exif_reg[5] <= intr_data_in[5];

	// Hardware update of IE4 has greater priority than software update.
           if (ie4_set == 1)                   exif_reg[6] <= 1;
      else if ((sfr_wr == 1) & (exif_cs == 1)) exif_reg[6] <= intr_data_in[6];

	// Hardware update of IE5 has greater priority than software update.
           if (ie5_set == 1)                   exif_reg[7] <= 1;
      else if ((sfr_wr == 1) & (exif_cs == 1)) exif_reg[7] <= intr_data_in[7];

      if (eicon_wr == 1)
      begin 
        smod1_reg <= intr_data_in[7];
        epfi      <= intr_data_in[5];
      end 

	// Hardware update of PFI has greater priority than software update.
           if (pfi_set  == 1) pfi_reg <= 1;
      else if (eicon_wr == 1) pfi_reg <= intr_data_in[4];

	// Hardware update of WDTI has greater priority than software update.
           if (wdti_set == 1) wdti_reg <= 1;
      else if (eicon_wr == 1) wdti_reg <= intr_data_in[3];
    end 
  end  //sfr_process


	// Aliases of IE register bits
  assign  ea  = ie_reg[7];
  assign  es0 = ie_reg[4];
  assign  es1 = ie_reg[6];
  assign  et0 = ie_reg[1];
  assign  et1 = ie_reg[3];
  assign  et2 = ie_reg[5];
  assign  ex0 = ie_reg[0];
  assign  ex1 = ie_reg[2];

	// Aliases of IP register bits
  assign  ps0 = ip_reg[4];
  assign  ps1 = ip_reg[6];
  assign  pt0 = ip_reg[1];
  assign  pt1 = ip_reg[3];
  assign  pt2 = ip_reg[5];
  assign  px0 = ip_reg[0];
  assign  px1 = ip_reg[2];

	// Aliases of TCON register bits
  assign  tcon_reg[0] = it0;
  assign  tcon_reg[1] = ie0;
  assign  tcon_reg[2] = it1;
  assign  tcon_reg[3] = ie1;
  assign  tcon_reg[4] = tr0;
  assign  tcon_reg[5] = tf0;
  assign  tcon_reg[6] = tr1;
  assign  tcon_reg[7] = tf1;

	// Aliases of EXIF register bits
  assign  ie5 = exif_reg[7];
  assign  ie4 = exif_reg[6];
  assign  ie3 = exif_reg[5];
  assign  ie2 = exif_reg[4];

	// Aliases of EIE register bits
  assign  ewdi = eie_reg[4];
  assign  ex5  = eie_reg[3];
  assign  ex4  = eie_reg[2];
  assign  ex3  = eie_reg[1];
  assign  ex2  = eie_reg[0];

	// Aliases of EIP register bits
  assign  pwdi = eip_reg[4];
  assign  px5  = eip_reg[3];
  assign  px4  = eip_reg[2];
  assign  px3  = eip_reg[1];
  assign  px2  = eip_reg[0];


  // external interrupt requests
  always @(posedge clk or negedge rst_n)
  begin : int_latch_process
    if (rst_n == 0)
    begin 
      x0_l1_n <= 0;
      x0_l2_n <= 0;
      x0_l3_n <= 0;
      x1_l1_n <= 0;
      x1_l2_n <= 0;
      x1_l3_n <= 0;
      x2_l1   <= 1;
      x2_l2   <= 1;
      x2_l3   <= 1;
      x3_l1_n <= 0;
      x3_l2_n <= 0;
      x3_l3_n <= 0;
      x4_l1   <= 1;
      x4_l2   <= 1;
      x4_l3   <= 1;
      x5_l1_n <= 0;
      x5_l2_n <= 0;
      x5_l3_n <= 0;
      pfi_l1  <= 0;
      pfi_l2  <= 0;
      wdti_l1 <= 0;
      wdti_l2 <= 0;
      wdti_l3 <= 0;
    end
    else
    begin
      x0_l1_n <= int0_n;	// Stage 1 synchronizers for all interrupts
      x1_l1_n <= int1_n;	// work at each clock.
      x2_l1   <= int2;
      x3_l1_n <= int3_n;
      x4_l1   <= int4;
      x5_l1_n <= int5_n;
      pfi_l1  <= pfi;
      wdti_l1 <= wdti;
      if (cycle == `c4)
      begin 
        x0_l2_n <= x0_l1_n;	// Stage 2 and Stage 3 synchronizers are 
        x1_l2_n <= x1_l1_n;	// enabled only at the end of clock phase C4.
        x2_l2   <= x2_l1;
        x3_l2_n <= x3_l1_n;
        x4_l2   <= x4_l1;
        x5_l2_n <= x5_l1_n;
        x0_l3_n <= x0_l2_n;
        x1_l3_n <= x1_l2_n;
        x2_l3   <= x2_l2;
        x3_l3_n <= x3_l2_n;
        x4_l3   <= x4_l2;
        x5_l3_n <= x5_l2_n;
        pfi_l2  <= pfi_l1;
        wdti_l2 <= wdti_l1;
        wdti_l3 <= wdti_l2;
      end 
    end 
  end   //int_latch_process


  // Interrupt Mask Logic
  assign  x0_req = ie0 & ex0;
  assign  x1_req = ie1 & ex1;
  assign  t0_req = (tf0 | tf0_set) & et0;
  assign  t1_req = (tf1 | tf1_set) & et1;
  assign  s0_req = (ri0 | ti0)  & es0;
  assign  t2_req = (tf2 | exf2) & et2;
  assign  x2_req = ie2 & ex2;
  assign  x3_req = ie3 & ex3;
  assign  x4_req = ie4 & ex4;
  assign  x5_req = ie5 & ex5;
  assign  s1_req = (ri1 | ti1) & es1;
  assign  wd_req = wdti_reg & ewdi;
  assign  pf_req = pfi_reg  & epfi;

  // priorities of requests
  assign  x0_hp_req = x0_req &  px0;
  assign  x0_lp_req = x0_req & ~px0;
  assign  x1_hp_req = x1_req &  px1;
  assign  x1_lp_req = x1_req & ~px1;
  assign  x2_hp_req = x2_req &  px2;
  assign  x2_lp_req = x2_req & ~px2;
  assign  x3_hp_req = x3_req &  px3;
  assign  x3_lp_req = x3_req & ~px3;
  assign  x4_hp_req = x4_req &  px4;
  assign  x4_lp_req = x4_req & ~px4;
  assign  x5_hp_req = x5_req &  px5;
  assign  x5_lp_req = x5_req & ~px5;
  assign  t0_hp_req = t0_req &  pt0;
  assign  t0_lp_req = t0_req & ~pt0;
  assign  t1_hp_req = t1_req &  pt1;
  assign  t1_lp_req = t1_req & ~pt1;
  assign  t2_hp_req = t2_req &  pt2;
  assign  t2_lp_req = t2_req & ~pt2;
  assign  s0_hp_req = s0_req &  ps0;
  assign  s0_lp_req = s0_req & ~ps0;
  assign  s1_hp_req = s1_req &  ps1;
  assign  s1_lp_req = s1_req & ~ps1;
  assign  wd_hp_req = wd_req &  pwdi;
  assign  wd_lp_req = wd_req & ~pwdi;

  // interrupt sources

  // High priority queue
  assign hp_src = (x0_hp_req == 1) ? `x0_src :
                  (t0_hp_req == 1) ? `t0_src :
                  (x1_hp_req == 1) ? `x1_src :
                  (t1_hp_req == 1) ? `t1_src :
                  (s0_hp_req == 1) ? `s0_src :
                  (t2_hp_req == 1) ? `t2_src :
                  (s1_hp_req == 1) ? `s1_src :
                  (x2_hp_req == 1) ? `x2_src :
                  (x3_hp_req == 1) ? `x3_src :
                  (x4_hp_req == 1) ? `x4_src :
                  (x5_hp_req == 1) ? `x5_src :
                                     `wd_src;

  // Low priority queue
  assign lp_src = (x0_lp_req == 1) ? `x0_src :
                  (t0_lp_req == 1) ? `t0_src :
                  (x1_lp_req == 1) ? `x1_src :
                  (t1_lp_req == 1) ? `t1_src :
                  (s0_lp_req == 1) ? `s0_src :
                  (t2_lp_req == 1) ? `t2_src :
                  (s1_lp_req == 1) ? `s1_src :
                  (x2_lp_req == 1) ? `x2_src :
                  (x3_lp_req == 1) ? `x3_src :
                  (x4_lp_req == 1) ? `x4_src :
                  (x5_lp_req == 1) ? `x5_src :
                                     `wd_src;

  // High-priority interrupt request generation.
  assign  hp_req = (x0_hp_req |
                    t0_hp_req |
                    x1_hp_req |
                    t1_hp_req |
                    s0_hp_req |
                    t2_hp_req |
                    s1_hp_req |
                    x2_hp_req |
                    x3_hp_req |
                    x4_hp_req |
                    x5_hp_req |
                    wd_hp_req) & ea  & (~(pf_req | iip2));

  // Low priority interrupt request generation.
  // The lp_req is enabled only when  there is no high-priority request,
  // and when there is no hi-pri request being serviced, and when there is
  // no low-pri request being serviced.
  assign  lp_req = (x0_lp_req |
                    t0_lp_req |
                    x1_lp_req |
                    t1_lp_req |
                    s0_lp_req |
                    t2_lp_req |
                    s1_lp_req |
                    x2_lp_req |
                    x3_lp_req |
                    x4_lp_req |
                    x5_lp_req |
                    wd_lp_req) & ea & (~(pf_req | iip2)) & (~(hp_req | iip1));


  // requests for all interrupt levels sampled at the begin of c3
  always @(posedge clk or negedge rst_n)
  begin : req_process
    if (rst_n == 0)
    begin 
      pf_req_l <= 0;
      hp_req_l <= 0;
      lp_req_l <= 0;
    end
    else
    begin
      if (cycle == `c2)
      begin 
        pf_req_l <= pf_req;
        hp_req_l <= hp_req;
        lp_req_l <= lp_req;
      end 
    end 
  end  //req_process


  // control signals for iip process
  assign  iip2_set = pf_req_l & int_ack;
  assign  iip2_clr = int_clr;
  assign  iip1_set = hp_req_l & int_ack;
  assign  iip1_clr = int_clr  & ~iip2;
  assign  iip0_set = lp_req_l & int_ack;
  assign  iip0_clr = int_clr  & (~(iip1 | iip2));



  always @(posedge clk or negedge rst_n)
  begin : iip_process
    if (rst_n == 0)
    begin 
      iip0  <= 0;
      iip1  <= 0;
      iip2  <= 0;
    end
    else
    begin
           if (iip0_set == 1) iip0 <= 1;
      else if (iip0_clr == 1) iip0 <= 0;

           if (iip1_set == 1) iip1 <= 1;
      else if (iip1_clr == 1) iip1 <= 0;

           if (iip2_set == 1) iip2 <= 1;
      else if (iip2_clr == 1) iip2 <= 0;
     end 
  end  //iip_process


  // compute output signals
  assign i_src = (pf_req == 1) ? `pf_src :
                 (hp_req == 1) ? hp_src  :
                                 lp_src;
  assign  int_src  = i_src_l;
  assign  int_req  = (pf_req_l & ~iip2) |
                     (hp_req_l & ~iip1) |
                     (lp_req_l & ~iip0);

  assign  intr_sfr_cs  = ie_cs   |
                         ip_cs   |
                         tcon_cs |
                         exif_cs |
                         eie_cs  |
                         eip_cs  |
                         eicon_cs;

  assign  ena_t0    = tr0;
  assign  ena_t1    = tr1;
  assign  smod1     = smod1_reg;
  assign  eicon_reg = {smod1_reg, 1'b1, epfi, pfi_reg, wdti_reg, 3'b000};


  // output mux
  assign intr_data_out = (ie_cs   == 1) ? ie_reg              :
                         (ip_cs   == 1) ? {1'b1, ip_reg}      :
                         (tcon_cs == 1) ? tcon_reg            :
                         (exif_cs == 1) ? {exif_reg, 4'b1000} :
                         (eie_cs  == 1) ? {3'b111, eie_reg}   :
                         (eip_cs  == 1) ? {3'b111, eip_reg}   :
                                          eicon_reg;

  // store source of actual interrupt
  always @(posedge clk or negedge rst_n)
  begin : i_src_latch_process
    if (rst_n == 0)
    begin 
      i_src_l <= 'b0;
    end
    else
    begin
      if (cycle == `c2)
        i_src_l <= i_src;
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
