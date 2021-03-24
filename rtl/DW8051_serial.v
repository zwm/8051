// $Id: DW8051_serial.v,v 1.1 1996/07/25 17:43:22 gina Exp $
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
// FILE: 	DW8051_serial.v
//
// AUTHOR:	Gregor Uhlaender
//
// ABSTRACT: 	DW8051 serial module (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        28.05.96        Verilog version created
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//      Bala Needamangalam
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_serial (clk,
                      rst_n,

                      // sfr bus:
                      sfr_addr,
                      serial_sfr_cs,
                      serial_data_out,
                      serial_data_in,
                      sfr_wr,

                      // baudrate input from timer:  
                      t1_ofl,

                      // baudrate inputs from timer2:
                      rclk,
                      tclk,
                      t2_ofl,

                      // signals from DW8051_cpu:
                      cycle,
                      smod,

                      // signals to DW8051_intr
                      ri,
                      ti,

                      // external IO:
                      rxd_out,
                      rxd_in,
                      txd
		      );

parameter base_addr  = 1;
 
 input clk;
 input rst_n;
 input [7:0]  sfr_addr;
 input [7:0]  serial_data_in;
 input sfr_wr;
 input t1_ofl;
 input rclk;
 input tclk;
 input t2_ofl;
 input [1:0]  cycle;
 input smod;
 input rxd_in;
 output serial_sfr_cs;
 output [7:0]  serial_data_out;
 output ri;
 output ti;
 output rxd_out;
 output txd;

//------------------------------------------------------------------------------
wire clk;
wire rst_n;
wire [7:0] sfr_addr;
wire [7:0] serial_data_in;
wire sfr_wr;
wire t1_ofl;
wire rclk;
wire tclk;
wire t2_ofl;
wire [1:0] cycle;
wire smod;
wire rxd_in;
wire serial_sfr_cs;
wire [7:0] serial_data_out;
wire ri;
wire ti;
reg  rxd_out;
reg  txd;

// type rx_state_type:
`define rx_idle       2'b00
`define rx_initialize 2'b01
`define rx_transfer   2'b10
`define rx_stop       2'b11

// type tx_state_type:
`define tx_idle         3'b000
`define tx_initialize   3'b001
`define tx_transfer_1st 3'b010
`define tx_transfer     3'b011
`define tx_stop1        3'b100
`define tx_stop2        3'b101

reg  [2:0] tx_state;
reg  [1:0] rx_state;
 
reg  [7:0] scon_reg;
wire [1:0] mode;
wire mode_change;
wire mode0;
wire mode1;
wire mode2;
wire mode3;
wire mode1or3;
 
wire sm2;
wire ren;
wire tb8;
wire rb8;
wire i_ti;
wire i_ri;
 
wire set_ti;
wire rx_accept;
reg  rx_update;
 
wire scon_cs;
wire sbuf_cs;
 
wire legal_sbuf_wr;
wire scon_wr;
 
wire clk_div2;
reg  clk_div12;
wire sync_clk;
reg  [1:0] count;
 
 
wire t_clk_x16;
wire t_clk_x16_m2;
reg  tx_m123_clk;
wire tx_t_clk_x16;
wire rx_t_clk_x16;		// sample clock for rxd_in
wire rx_t_clk;
reg  rxd_l0;			// latched rxd_in
reg  rxd_l0b;
reg  rxd_l1;			// latched rxd_l0
reg  rxd_l2;			// latched rxd_l1
reg  rxd_l3;			// latched rxd_l2
wire rxd_event;			// set when high to low transition detected
 
wire clr_rx_cnt_n;
wire ld_tx_cnt_n;
 
wire [3:0] rx_cnt;
wire [3:0] tx_cnt;
 
reg  store_t1_ofl;
 
wire tx_ser_in;
wire [1:0] tx_par_in_msb;
wire [9:0] tx_par_in;
wire [9:0] tx_par_in_rev;
wire tx_load_n;
wire tx_shift_n;
wire [9:0] tx_sbuf_reg;
wire [9:0] tx_reg_rev;
 
wire rx_ser_in;
wire [8:0] rx_par_in;
wire [8:0] rx_par_in_rev;
wire rx_load_n;
wire rx_shift_n;
wire [8:0] rx_reg;
wire [8:0] rx_reg_rev;
reg  [7:0] rx_sbuf_reg;
wire rx_tercnt;
 
wire rx_clk;
wire tx_clk;
 
wire tx_tercnt;
 
wire zero_det_n;
 
reg  shift_clk;
 
wire tx_busy;
wire rx_busy;
wire port_busy;
 
wire send;
wire receive;
wire data;
wire stop_tx;
wire stop_rx;
 
wire rx_bit;
wire rx_cmp_3;
 
wire high;
wire [3:0] low_4;
wire [3:0] tx_ld_data;
 
reg  ti_just_set;
reg  ri_just_set;

wire [7:0] base_addr_p_1;

//------------------------------------------------------------------------------

  // define constant signals
  assign  high       = 1;
  assign  low_4      = 'b0;
  assign  tx_ld_data = 4'b1111;

  assign base_addr_p_1 = (base_addr + 1);


  // sfr registers:
  assign  mode = scon_reg[7:6];
  assign  sm2  = scon_reg[5];
  assign  ren  = scon_reg[4];
  assign  tb8  = scon_reg[3];
  assign  rb8  = scon_reg[2];
  assign  i_ti = scon_reg[1];
  assign  i_ri = scon_reg[0];

  assign scon_cs  = (sfr_addr ==  base_addr)    ? 1 : 0;
  assign sbuf_cs  = (sfr_addr == base_addr_p_1) ? 1 : 0;

  // decode mode
  assign  mode0    = ~(mode[0] |  mode[1]);
  assign  mode1    =   mode[0] & ~mode[1];
  assign  mode2    =  ~mode[0] &  mode[1];
  assign  mode3    =   mode[0] &  mode[1];
  assign  mode1or3 =   mode[0];

  assign tx_busy    = (tx_state != `tx_idle) ? 1 : 0;
  assign rx_busy    = (rx_state != `rx_idle) ? 1 : 0;
  assign port_busy  = tx_busy | rx_busy;

  assign  legal_sbuf_wr = sbuf_cs & sfr_wr & ~tx_busy;

  assign  scon_wr = scon_cs & sfr_wr;



  always @(posedge clk or negedge rst_n)
  begin : sfr_process
    if (rst_n == 0)
    begin 
      scon_reg    <= 'b0;
      rx_sbuf_reg <= 'b0;
      ti_just_set <= 0;
      ri_just_set <= 0;
    end
    else
    begin
      if (cycle == `c3)
      begin 
        ti_just_set  <= set_ti;
        if (mode0 == 1) ri_just_set <= rx_accept;
      end 

      // scon register
           if (rx_accept == 1) scon_reg[0] <= 1;
      else if ((scon_wr == 1) & (ri_just_set == 0))
                               scon_reg[0] <= serial_data_in[0];

           if (set_ti == 1)    scon_reg[1] <= 1;
      else if ((scon_wr == 1) & (ti_just_set == 0))
                               scon_reg[1] <= serial_data_in[1];

           if ((rx_accept == 1) & (mode0 == 0))
                               scon_reg[2] <= rx_bit;
      else if (scon_wr == 1)   scon_reg[2] <= serial_data_in[2];

      if (scon_wr == 1)   scon_reg[7:3] <= serial_data_in[7:3];

      // rx_sbuf register
      if (rx_update == 1) rx_sbuf_reg   <= rx_reg[7:0];
     end 
  end  //sfr_process


  assign tx_load_n      = ~legal_sbuf_wr;
  assign tx_par_in_msb  = ((mode0 == 1) | (mode1 == 1)) ? 2'b01 :
                                                          {1'b1, tb8};
  assign  tx_par_in     = {tx_par_in_msb, serial_data_in};

  //----------------------------------------------------------------------------

  // tx shift register:
  DW8051_shftreg #(10) u0
                    (.clk     (clk),
                     .s_in    (tx_ser_in),
                     .p_in    (tx_par_in_rev),
                     .shift_n (tx_shift_n),
                     .load_n  (tx_load_n),
                     .reset_n (rst_n),
                     .p_out   (tx_reg_rev));

  // reverse order of bits: DW8051_shftreg assumes MSB first
  //                        but data are transmitted LSB first
  assign  tx_par_in_rev[0] = tx_par_in[9];
  assign  tx_par_in_rev[1] = tx_par_in[8];
  assign  tx_par_in_rev[2] = tx_par_in[7];
  assign  tx_par_in_rev[3] = tx_par_in[6];
  assign  tx_par_in_rev[4] = tx_par_in[5];
  assign  tx_par_in_rev[5] = tx_par_in[4];
  assign  tx_par_in_rev[6] = tx_par_in[3];
  assign  tx_par_in_rev[7] = tx_par_in[2];
  assign  tx_par_in_rev[8] = tx_par_in[1];
  assign  tx_par_in_rev[9] = tx_par_in[0];
  assign  tx_sbuf_reg[0]   = tx_reg_rev[9];
  assign  tx_sbuf_reg[1]   = tx_reg_rev[8];
  assign  tx_sbuf_reg[2]   = tx_reg_rev[7];
  assign  tx_sbuf_reg[3]   = tx_reg_rev[6];
  assign  tx_sbuf_reg[4]   = tx_reg_rev[5];
  assign  tx_sbuf_reg[5]   = tx_reg_rev[4];
  assign  tx_sbuf_reg[6]   = tx_reg_rev[3];
  assign  tx_sbuf_reg[7]   = tx_reg_rev[2];
  assign  tx_sbuf_reg[8]   = tx_reg_rev[1];
  assign  tx_sbuf_reg[9]   = tx_reg_rev[0];



  // rx shift register:
  DW8051_shftreg #(9) u1
                   (.clk     (clk),
                    .s_in    (rx_ser_in),
                    .p_in    (rx_par_in_rev),
                    .shift_n (rx_shift_n),
                    .load_n  (rx_load_n),
                    .reset_n (rst_n),
                    .p_out   (rx_reg_rev));

  // reverse order of bits: DW8051_shftreg assumes MSB first
  //                        but data are received LSB first
  assign  rx_par_in_rev[0] = rx_par_in[8];
  assign  rx_par_in_rev[1] = rx_par_in[7];
  assign  rx_par_in_rev[2] = rx_par_in[6];
  assign  rx_par_in_rev[3] = rx_par_in[5];
  assign  rx_par_in_rev[4] = rx_par_in[4];
  assign  rx_par_in_rev[5] = rx_par_in[3];
  assign  rx_par_in_rev[6] = rx_par_in[2];
  assign  rx_par_in_rev[7] = rx_par_in[1];
  assign  rx_par_in_rev[8] = rx_par_in[0];
  assign  rx_reg[0] = rx_reg_rev[8];
  assign  rx_reg[1] = rx_reg_rev[7];
  assign  rx_reg[2] = rx_reg_rev[6];
  assign  rx_reg[3] = rx_reg_rev[5];
  assign  rx_reg[4] = rx_reg_rev[4];
  assign  rx_reg[5] = rx_reg_rev[3];
  assign  rx_reg[6] = rx_reg_rev[2];
  assign  rx_reg[7] = rx_reg_rev[1];
  assign  rx_reg[8] = rx_reg_rev[0];

  //----------------------------------------------------------------------------

  // clock generation
  assign clk_div2 = ~cycle[0];
  assign sync_clk = (mode0         == 0)                       ? 0 :
                    (port_busy     == 1)                       ? 0 :
                    (legal_sbuf_wr == 1)                       ? 1 :
                    (scon_wr       == 0)                       ? 0 :
                  // ren goes high and ri is low and remains low
                    ((ren  == 0) & (serial_data_in[4] == 1) &
                     (i_ri == 0) & (serial_data_in[0] == 0))   ? 1 :
                  // ri goes low and ren is high and remains high
                    ((i_ri == 1) & (serial_data_in[0] == 0) &
                     (ren  == 1) & (serial_data_in[4] == 1))   ? 1 :
                  // ri goes low and ren goes high
                    ((i_ri == 1) & (serial_data_in[0] == 0) &
                     (ren  == 0) & (serial_data_in[4] == 1))   ? 1 :
                                                                 0;


  always @(posedge clk or negedge rst_n)
  begin : clk_div_process
    if (rst_n == 0)
    begin 
      count     <= 2'b00;
      clk_div12 <= 0;
    end
    else
    begin
      if (sync_clk == 1)
      begin 
        count     <= 2'b00;
        clk_div12 <= 0;
      end
      else if (cycle == `c3)
      begin 
        count[0]  <= ~(count[0] | count[1]);
        count[1]  <= count[0];
        clk_div12 <= count[1];
      end
      else clk_div12 <= 0;
    end 
  end  //clk_div_process


  always @(posedge clk or negedge rst_n)
  begin : t1_ofl_div_process
    if (rst_n == 0)
    begin 
      store_t1_ofl <= 0;
    end
    else
    begin
      if (t1_ofl == 1)
      begin 
        store_t1_ofl <= ~store_t1_ofl;
      end 
    end 
  end   //t1_ofl_div_process


  assign t_clk_x16    = (mode0 == 1)                        ? 0      :
                        ((smod == 1) | (store_t1_ofl == 0)) ? t1_ofl :
                                                            0;

  assign t_clk_x16_m2 =  (smod == 1)                   ? clk_div2 :
                        ((smod == 0) & (cycle == `c1)) ? 1        :
                                                         0;

  assign rx_t_clk_x16 = (mode2 == 1) ? t_clk_x16_m2 :
                        (rclk  == 0) ? t_clk_x16    :
                                       t2_ofl;

  assign tx_t_clk_x16 = (mode2 == 1) ? t_clk_x16_m2 :
                        (tclk  == 0) ? t_clk_x16    :
                                       t2_ofl;


  // input latches
  always @(posedge clk or negedge rst_n)
  begin : inp_latch_process
    if (rst_n == 0)
    begin 
      rxd_l0  <= 0;
      rxd_l0b <= 0;
      rxd_l1  <= 0;
      rxd_l2  <= 0;
      rxd_l3  <= 0;
    end
    else
    begin
      rxd_l0  <= rxd_in;
      rxd_l0b <= rxd_l0;
      if (rx_t_clk_x16 == 1)
      begin 
        if (mode1or3 == 1) rxd_l1 <= rxd_l0b;
        else               rxd_l1 <= rxd_l0;
      end 
      if (rx_t_clk_x16 == 1)
      begin 
        rxd_l2 <= rxd_l1;
        rxd_l3 <= rxd_l2;
      end 
    end 
  end  //inp_latch_process


  // detect high to low transition on rxd input
  assign  rxd_event   = rxd_l2 & ~rxd_l1;
  assign clr_rx_cnt_n = (rx_state == `rx_idle) ? ~rxd_event : 1;

  assign  mode_change = ((mode[0] ^ serial_data_in[6]) |
                         (mode[1] ^ serial_data_in[7])) & scon_wr;

  assign  ld_tx_cnt_n = ~(mode_change & serial_data_in[6]);


  // divide rx_t_clk_x16 by 16
  DW8051_updn_ctr #(4) u2
                    (.up_dn  (high),		// up counter
                     .load   (clr_rx_cnt_n),	// active low
                     .data   (low_4),
                     .cen    (rx_t_clk_x16),
                     .clk    (clk),
                     .reset  (rst_n),		// active low
                     .count  (rx_cnt),
                     .tercnt (rx_tercnt));

  // divide tx_t_clk_x16 by 16
  DW8051_updn_ctr #(4) u3
                    (.up_dn  (high),		// up counter
                     .load   (ld_tx_cnt_n),	// active low
                     .data   (tx_ld_data),
                     .cen    (tx_t_clk_x16),
                     .clk    (clk),
                     .reset  (rst_n),		// active low
                     .count  (tx_cnt),
                     .tercnt (tx_tercnt));


  always @ (posedge clk or negedge rst_n)
  begin : tx_m123_clk_process
    if (rst_n == 0)
    begin 
      tx_m123_clk <= 0;
    end
    else
    begin
      if ((tx_tercnt == 1) & (tx_t_clk_x16 == 1)) tx_m123_clk <= 1;
      else if (cycle == `c1)                      tx_m123_clk <= 0;
    end 
  end  //tx_m123_clk_process


  //----------------------------------------------------------------------------

  // generate output signals:
  always @(mode0 or cycle or sm2 or count or clk_div12)
  begin : shift_clk_process
    if (mode0 == 1)
    begin 
      if (sm2 == 1)
      begin 
        if ((cycle == `c1) | (cycle == `c4)) shift_clk <= 1;
        else                                 shift_clk <= 0;
      end
      else
      begin
        if (((cycle == `c3) & (count == 2'b10)) |
            (clk_div12 == 1) |
            ((cycle == `c1) & (count == 2'b00))) shift_clk <= 1;
        else                                     shift_clk <= 0;
      end 
    end
    else shift_clk <= 0;
  end  //shift_clk_process


  always @(posedge clk or negedge rst_n)
  begin : out_process
    if (rst_n == 0)
    begin 
      txd     <= 1;
      rxd_out <= 1;
    end
    else
    begin
      if (mode0 == 1) txd <= ((send | receive) & shift_clk) |
                              (~(send | receive));
      else            txd <= (data & tx_sbuf_reg[0]) | ~send;

      if ((mode0 == 1) & (send == 1)) rxd_out <= tx_sbuf_reg[0];
      else                            rxd_out <= 1;
    end 
  end  //out_process

  assign ri = i_ri;
  assign ti = i_ti;

  assign serial_sfr_cs  = scon_cs | sbuf_cs;

  assign serial_data_out = (sbuf_cs == 1) ? rx_sbuf_reg : scon_reg;

  //----------------------------------------------------------------------------
  // tx control:

  assign tx_clk = ((mode0 == 1) & (sm2 == 0))                  ? clk_div12   :
                  ((mode0 == 1) & (sm2 == 1) & (cycle == `c4)) ? 1           :
                  (cycle == `c1)                               ? tx_m123_clk :
                                                                 0;

  assign tx_shift_n = (tx_state == `tx_idle)                         ? 1 :
                      (tx_state == `tx_initialize)                   ? 1 :
                     ((tx_state == `tx_transfer_1st) & (mode0 == 0)) ? 1 :
                                                                       ~tx_clk;

  assign zero_det_n = (tx_sbuf_reg[9:2] != 8'b00000000) ? 1 : 0;

  assign  tx_ser_in = 0;

  assign send  = (tx_state == `tx_transfer_1st) ? 1 :
                 (tx_state == `tx_transfer)     ? 1 :
                                                  0;

  assign data  = (tx_state == `tx_transfer) ? 1 : 0;

  assign set_ti = ((mode0 == 1) & (tx_state == `tx_stop1) &
                   (cycle == `c3))                           ? 1 :
                  ((tx_state == `tx_stop2) & (cycle == `c3)) ? 1 :
                                                                0;

  assign stop_tx = (mode_change == 1) ? 1 : 0;


  always @(posedge clk or negedge rst_n)
  begin : tx_control_process
    if (rst_n == 0)
    begin 
      tx_state  <= `tx_idle;
    end
    else
    begin
      case (tx_state )
        `tx_idle        : begin
                            if (legal_sbuf_wr == 1) tx_state <= `tx_initialize;
                          end
        `tx_initialize  : begin
                                 if (stop_tx == 1) tx_state <= `tx_idle;
                            else if (tx_clk  == 1) tx_state <= `tx_transfer_1st;
                          end
        `tx_transfer_1st: begin
                                 if (stop_tx == 1) tx_state <= `tx_idle;
                            else if (tx_clk  == 1) tx_state <= `tx_transfer;
                          end
        `tx_transfer    : begin
                                 if (stop_tx == 1)      tx_state <= `tx_idle;
                            else if ((zero_det_n == 0) &
                                     (tx_shift_n == 0)) tx_state <= `tx_stop1;
                          end
        `tx_stop1       : begin
                                 if (stop_tx == 1) tx_state <= `tx_idle;
                            else if (cycle == `c3)
                            begin 
                              if (mode0 == 1) tx_state <= `tx_idle;
                              else            tx_state <= `tx_stop2;
                            end 
                          end
        `tx_stop2       : begin
                                 if (stop_tx == 1) tx_state <= `tx_idle;
                            else if (cycle == `c3) tx_state <= `tx_idle;
                          end
      endcase
    end 
  end  //tx_control_process

  //----------------------------------------------------------------------------
  // rx control:

  assign rx_clk = ((mode0 == 1) & (sm2 == 0))                  ? clk_div12 :
                  ((mode0 == 1) & (sm2 == 1) & (cycle == `c1)) ? 1         :
                                                                 rx_t_clk;

  assign rx_load_n  = ((rx_state == `rx_initialize) & (rx_clk == 1)) ? 0 : 1;

  assign rx_ser_in  = (mode0 == 1) ? rxd_l0 : rx_bit;

  assign rx_par_in  = (mode0 == 1) ? 9'b001111111 : 9'b011111111;

  assign receive    = (rx_state == `rx_transfer) ? 1 : 0;

  assign rx_shift_n = (rx_state == `rx_transfer) ? ~rx_clk    :
                      (mode0    == 1)            ? ~rx_accept :
                                                   1;

  assign rx_accept  = (ren == 0)                                       ? 0 :
                      ((mode0 == 1) & (cycle == `c3) &
                       (rx_state == `rx_stop))                         ? 1 :
                      (mode0 == 1)                                     ? 0 :
                      (cycle != `c1 )                                  ? 0 :
                      (~(((rx_state  == `rx_transfer) &
                          (rx_clk    == 1) &
                          (rx_reg[0] == 0)) | (rx_state == `rx_stop))) ? 0 :
                      (i_ri == 1)                                      ? 0 :
                      ((sm2 == 0) | (rx_bit == 1))                     ? 1 :
                                                                         0;

  // 2 of 3 majority
  assign rx_cmp_3  = (mode2 == 1) ? rxd_l3 : rxd_l0b;

  assign rx_bit = (rxd_l1 & rxd_l2) | (rxd_l2 & rx_cmp_3) | 
		  (rx_cmp_3 & rxd_l1);

  assign rx_t_clk = ((rx_cnt == 4'b1000) & (mode1or3 == 1)) ? rx_t_clk_x16 :
                    ((rx_cnt == 4'b1001) & (mode1or3 == 0)) ? rx_t_clk_x16 :
                                                              0;

  assign stop_rx  = (ren == 0)         ? 1 :
                    (mode_change == 1) ? 1 :
                                         0;



  always @(posedge clk or negedge rst_n)
  begin : rx_control_process
    if (rst_n == 0)
    begin 
      rx_update <= 0;
      rx_state  <= `rx_idle;
    end
    else
    begin
      rx_update <= rx_accept;

      case (rx_state)
        `rx_idle       : begin
                           if (mode0 == 1)
                           begin 
                             if ((ren == 1) & (i_ri == 0))
                               rx_state <= `rx_initialize;
                           end
                           else
                           begin
                             if ((rxd_event == 1) & (ren == 1))
                               rx_state <= `rx_initialize;
                           end 
                         end
        `rx_initialize : begin
                           if (stop_rx == 1) rx_state <= `rx_idle;
                           else if (rx_clk == 1)
                           begin 
                             if ((mode0 == 0) & (rx_bit == 1))
                                  rx_state <= `rx_idle;
                             else rx_state <= `rx_transfer;
                           end 
                         end
        `rx_transfer   : begin
                           if (stop_rx == 1) rx_state <= `rx_idle;
                           else if ((rx_reg[0] == 0) & (rx_clk == 1))
                           begin 
                             if ((mode0 == 1) | ~(cycle == `c1))
                                  rx_state <= `rx_stop;
                             else rx_state <= `rx_idle;
                           end 
                         end
        `rx_stop       : begin
                           if  ((stop_rx == 1) |
                                ((mode0 == 1) & (cycle == `c3)) |
                                ((mode0 == 0) & (cycle == `c1)))
                           begin 
                             rx_state  <= `rx_idle;
                           end 
                         end
      endcase
    end 
  end   //rx_control_process


endmodule
