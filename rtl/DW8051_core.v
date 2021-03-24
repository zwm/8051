// $Id: DW8051_core.v,v 1.1 1996/07/25 17:42:36 gina Exp $
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
// FILE: DW8051_core.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 core module (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        10.06.96        Verilog version created
//	L.Rieder	17.07.96	ri0,ti0,ri1,ti1 = 0 if modules not
//					present
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//			Dec 20.97	Internal RAM and ROM interface signals
//					added to core I/Os. Internal ROM 
//					instantiation removed. Translated from
//					Ludwig's VHDL changes.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"


module DW8051_core (clk, 
		    por_n,
                    rst_in_n,
                    rst_out_n,
                    test_mode_n,

                    stop_mode_n,
                    idle_mode_n,
    
                    sfr_addr,
                    sfr_data_out,
                    sfr_data_in,
                    sfr_wr,
                    sfr_rd,

                    mem_addr,
                    mem_data_out,
                    mem_data_in,
                    mem_wr_n,
                    mem_rd_n,
                    mem_pswr_n,
                    mem_psrd_n,
                    mem_ale,
                    mem_ea_n,

                    int0_n,		// External Interrupt 0, std
                    int1_n,		// External Interrupt 1, std
                    int2,		// External Interrupt 2, ext
                    int3_n,		// External Interrupt 3, ext
                    int4,		// External Interrupt 4, ext
                    int5_n,		// External Interrupt 5, ext

                    pfi,		// power fail interrupt, ext
                    wdti,		// watchdog timer intr, ext

                    rxd0_in,		// serial port 0 input
                    rxd0_out,		// serial port 0 output
                    txd0,		// serial port 0 output

                    rxd1_in,		// serial port 1 input
                    rxd1_out,		// serial port 1 output
                    txd1,		// serial port 1 output

                    t0,			// Timer 0 external input
                    t1,			// Timer 1 external input
                    t2,			// Timer/Counter2 ext.input
                    t2ex,		// Timer/Counter2 capt./reload
                    t0_out,		// Timer/Counter0 ext. output
                    t1_out,		// Timer/Counter1 ext. output
                    t2_out,		// Timer/Counter2 ext. output

                    port_pin_reg_n,
                    p0_mem_reg_n,
                    p0_addr_data_n,
                    p2_mem_reg_n,

		    iram_addr,
		    iram_data_out,
		    iram_data_in,
		    iram_rd_n,
		    iram_we1_n,
		    iram_we2_n,

		    irom_addr,
		    irom_data_out,
		    irom_rd_n,
		    irom_cs_n
// synopsys dc_script_begin
// set_design_license  {DesignWare-Foundation} -quiet
// synopsys dc_script_end
		    );

 input clk;
 input por_n;
 input rst_in_n;
 input test_mode_n;
 input [7:0] sfr_data_in;
 input [7:0] mem_data_in;
 input mem_ea_n;
 input int0_n;
 input int1_n;
 input int2;
 input int3_n;
 input int4;
 input int5_n;
 input pfi;
 input wdti;
 input rxd0_in;
 input rxd1_in;
 input t0;
 input t1;
 input t2;
 input t2ex;
 input [7:0] iram_data_out;
 input [7:0] irom_data_out;
 output rst_out_n;
 output stop_mode_n;
 output idle_mode_n;
 output [7:0] sfr_addr;
 output [7:0] sfr_data_out;
 output sfr_wr;
 output sfr_rd;
 output [15:0] mem_addr;
 output [7:0]  mem_data_out;
 output mem_wr_n;
 output mem_rd_n;
 output mem_pswr_n;
 output mem_psrd_n;
 output mem_ale;
 output rxd0_out;
 output txd0;
 output rxd1_out;
 output txd1;
 output t0_out;
 output t1_out;
 output t2_out;
 output port_pin_reg_n;
 output p0_mem_reg_n;
 output p0_addr_data_n;
 output p2_mem_reg_n;
 output [7:0] iram_addr;
 output [7:0] iram_data_in;
 output iram_rd_n;
 output	iram_we1_n;
 output iram_we2_n;
 output [15:0] irom_addr;
 output irom_rd_n;
 output irom_cs_n;

//------------------------------------------------------------------------------
wire clk;
wire por_n;
wire rst_in_n;
wire test_mode_n;
wire [7:0] sfr_data_in;
wire [7:0] mem_data_in;
wire mem_ea_n;
wire int0_n;
wire int1_n;
wire int2;
wire int3_n;
wire int4;
wire int5_n;
wire pfi;
wire wdti;
wire rxd0_in;
wire rxd1_in;
wire t0;
wire t1;
wire t2;
wire t2ex;
wire rst_out_n;
wire stop_mode_n;
wire idle_mode_n;
wire [7:0] sfr_addr;
wire [7:0] sfr_data_out;
wire sfr_wr;
wire sfr_rd;
wire [15:0] mem_addr;
wire [7:0]  mem_data_out;
wire mem_wr_n;
wire mem_rd_n;
wire mem_pswr_n;
wire mem_psrd_n;
wire mem_ale;
wire rxd0_out;
wire txd0;
wire rxd1_out;
wire txd1;
wire t0_out;
wire t1_out;
wire t2_out;
wire port_pin_reg_n;
wire p0_mem_reg_n;
wire p0_addr_data_n;
wire p2_mem_reg_n;


//---------------
// local signals:
//---------------
wire low;
 
wire t_rst_out_n;
 
wire timer2_sfr_cs;
wire t_timer2_sfr_cs;
wire [7:0] timer2_data_out;
wire [7:0] t_timer2_data_out;
wire t_t2_out;

wire serial0_sfr_cs;
wire t_serial0_sfr_cs;
wire [7:0] serial0_data_out;
wire [7:0] t_serial0_data_out;
wire serial1_sfr_cs;
wire t_serial1_sfr_cs;
wire [7:0] serial1_data_out;
wire [7:0] t_serial1_data_out;
 
wire [7:0] int_sfr_addr;
wire [7:0] int_sfr_data_out;
wire [7:0] int_sfr_data_in;
wire int_sfr_wr;
wire int_sfr_cs;
 
wire [15:0] t_mem_addr;
wire [7:0]  t_mem_data_out;
wire t_mem_psrd_n;
 
wire timer_sfr_cs;
wire [7:0] timer_data_out;
 
wire intr_sfr_cs;
wire [7:0] intr_data_out;
wire       intr_sfr_cs_0;
wire [7:0] intr_data_out_0;
wire       intr_sfr_cs_1;
wire [7:0] intr_data_out_1;
 
wire [2:0] tm;
wire t0m, t1m, t2m;

wire ena_t0;
wire ena_t1;
wire ena_t0_0;
wire ena_t1_0;
wire ena_t0_1;
wire ena_t1_1;

wire tf0_set, tf1_set;
wire t1_ofl;

wire rclk, tclk;
wire t_rclk, t_tclk;
wire t2_ofl;
wire t_t2_ofl;
wire tf2, exf2;
wire t_tf2, t_exf2;

wire smod0;
wire ri0, ti0;
wire t_ri0, t_ti0;
wire ri1, ti1;
wire t_ri1, t_ti1;
wire t_rxd0_out;
wire t_txd0;
wire t_rxd1_out;
wire t_txd1;

wire smod1;
wire smod1_0;
wire smod1_1;
 
wire [1:0] cpu_cycle;
wire cpu_int_req;
wire [3:0] cpu_int_src;
wire cpu_int_ack;
wire cpu_int_clr;
 
wire       cpu_int_req_0;
wire [3:0] cpu_int_src_0;
wire       cpu_int_req_1;
wire [3:0] cpu_int_src_1;
 
wire [15:0] irom_addr;
wire irom_rd_n;
wire irom_cs_n;
wire [7:0] int_rom_data_out;
wire int_rom_rd_n;
wire int_rom_cs_n;

//------------------------------------------------------------------------------

  assign low = 0;

  //------------------------------
  // Implementation of CPU kernel:
  //------------------------------
  DW8051_cpu #(`ram_256, `rom_addr_size, `extd_intr) i_cpu
             (.clk              (clk),
              .por_n            (por_n),
              .rst_in_n         (rst_in_n),
              .rst_out_n        (t_rst_out_n),
              .test_mode_n      (test_mode_n),
              .cycle            (cpu_cycle),
              .stop_mode_n      (stop_mode_n),
              .idle_mode_n      (idle_mode_n),
              .int_sfr_addr     (int_sfr_addr),
              .int_sfr_data_out (int_sfr_data_out),
              .int_sfr_data_in  (int_sfr_data_in),
              .int_sfr_wr       (int_sfr_wr),
              .int_sfr_cs       (int_sfr_cs),
              .ext_sfr_addr     (sfr_addr),
              .ext_sfr_data_out (sfr_data_out),
              .ext_sfr_data_in  (sfr_data_in),
              .ext_sfr_wr       (sfr_wr),
              .ext_sfr_rd       (sfr_rd),
              .mem_addr         (t_mem_addr),
              .mem_data_out     (t_mem_data_out),
              .mem_data_in      (mem_data_in),
              .mem_wr_n         (mem_wr_n),
              .mem_rd_n         (mem_rd_n),
              .mem_pswr_n       (mem_pswr_n),
              .mem_psrd_n       (t_mem_psrd_n),
              .mem_ale          (mem_ale),
              .mem_ea_n         (mem_ea_n),
              .port_pin_reg_n   (port_pin_reg_n),
              .p0_mem_reg_n     (p0_mem_reg_n),
              .p0_addr_data_n   (p0_addr_data_n),
              .p2_mem_reg_n     (p2_mem_reg_n),
	      .iram_addr	(iram_addr),
	      .iram_data_out	(iram_data_out),
	      .iram_data_in	(iram_data_in),
	      .iram_rd_n	(iram_rd_n),
	      .iram_we1_n	(iram_we1_n),
	      .iram_we2_n	(iram_we2_n),
              .smod             (smod0),
              .tm               (tm),
              .int_req          (cpu_int_req),
              .int_src          (cpu_int_src),
              .int_ack          (cpu_int_ack),
              .int_clr          (cpu_int_clr),
              .int_rom_data_in  (int_rom_data_out),
              .int_rom_rd_n     (int_rom_rd_n),
              .int_rom_cs_n     (int_rom_cs_n));

  assign  t0m = tm[0];
  assign  t1m = tm[1];
  assign  t2m = tm[2];

  //------------------------
  // Internal ROM Interface:
  //------------------------
  assign irom_addr = t_mem_addr;
  assign irom_rd_n = int_rom_rd_n;
  assign irom_cs_n = int_rom_cs_n;
  assign int_rom_data_out = (`rom_addr_size == 0) ? 'b0 : irom_data_out;


  //--------------------------
  // Timer 0/1 implementation:
  //--------------------------
  DW8051_timer i_timer
               (.clk            (clk),
                .rst_n          (t_rst_out_n),
                .sfr_addr       (int_sfr_addr),
                .timer_sfr_cs   (timer_sfr_cs),
                .timer_data_out (timer_data_out),
                .timer_data_in  (int_sfr_data_out),
                .sfr_wr         (int_sfr_wr),
                .t0             (t0),
                .t1             (t1),
                .int0_n         (int0_n),
                .int1_n         (int1_n),
                .cycle          (cpu_cycle),
                .t0m            (t0m),
                .t1m            (t1m),
                .ena_t0         (ena_t0),
                .ena_t1         (ena_t1),
                .tf0_set        (tf0_set),
                .tf1_set        (tf1_set),
                .t1_ofl         (t1_ofl),
                .t0_out         (t0_out),
                .t1_out         (t1_out));


  //------------------------------------
  // Implementation of interrupt control
  // unit (normal: extd_intr=0,
  // extended: extd_intr=1).
  //------------------------------------

// App Builder controls for conditional instantiation.
// reuse-pragma startSub SmallInterruptUnit [parameter_conditional_text %subText {@extd_intr == 0}]
// reusepragma attr GenerateIf[0] {@extd_intr == 0}
  DW8051_intr_0 i_intr0
                (.clk           (clk),
                 .rst_n         (t_rst_out_n),
                 .sfr_addr      (int_sfr_addr),
                 .intr_sfr_cs   (intr_sfr_cs_0),
                 .intr_data_out (intr_data_out_0),
                 .intr_data_in  (int_sfr_data_out),
                 .sfr_wr        (int_sfr_wr),
                 .cycle         (cpu_cycle),
                 .int_req       (cpu_int_req_0),
                 .int_src       (cpu_int_src_0[2:0]),
                 .int_ack       (cpu_int_ack),
                 .int_clr       (cpu_int_clr),
                 .int0_n        (int0_n),
                 .int1_n        (int1_n),
                 .tf0_set       (tf0_set),
                 .tf1_set       (tf1_set),
                 .ri0           (ri0),
                 .ti0           (ti0),
                 .tf2           (tf2),
                 .exf2          (exf2),
                 .ena_t0        (ena_t0_0),
                 .ena_t1        (ena_t1_0),
                 .smod1         (smod1_0));
// reuse-pragma endSub SmallInterruptUnit

  assign cpu_int_src_0[3] = 0;


// App Builder controls for conditional instantiation.
// reuse-pragma startSub BigInterruptUnit [parameter_conditional_text %subText {@extd_intr == 1}]
// reusepragma attr GenerateIf[0] {@extd_intr == 1}
  DW8051_intr_1 i_intr1
                (.clk           (clk),
                 .rst_n         (t_rst_out_n),
                 .sfr_addr      (int_sfr_addr),
                 .intr_sfr_cs   (intr_sfr_cs_1),
                 .intr_data_out (intr_data_out_1),
                 .intr_data_in  (int_sfr_data_out),
                 .sfr_wr        (int_sfr_wr),
                 .cycle         (cpu_cycle),
                 .int_req       (cpu_int_req_1),
                 .int_src       (cpu_int_src_1),
                 .int_ack       (cpu_int_ack),
                 .int_clr       (cpu_int_clr),
                 .int0_n        (int0_n),
                 .int1_n        (int1_n),
                 .int2          (int2),
                 .int3_n        (int3_n),
                 .int4          (int4),
                 .int5_n        (int5_n),
                 .tf0_set       (tf0_set),
                 .tf1_set       (tf1_set),
                 .ri0           (ri0),
                 .ti0           (ti0),
                 .ri1           (ri1),
                 .ti1           (ti1),
                 .tf2           (tf2),
                 .exf2          (exf2),
                 .pfi           (pfi),
                 .wdti          (wdti),
                 .ena_t0        (ena_t0_1),
                 .ena_t1        (ena_t1_1),
                 .smod1         (smod1_1));
// reuse-pragma endSub BigInterruptUnit

  assign intr_sfr_cs   = (`extd_intr == 1) ? intr_sfr_cs_1   : intr_sfr_cs_0;
  assign intr_data_out = (`extd_intr == 1) ? intr_data_out_1 : intr_data_out_0;
  assign cpu_int_req   = (`extd_intr == 1) ? cpu_int_req_1   : cpu_int_req_0;
  assign cpu_int_src   = (`extd_intr == 1) ? cpu_int_src_1   : cpu_int_src_0;
  assign ena_t0        = (`extd_intr == 1) ? ena_t0_1        : ena_t0_0;
  assign ena_t1        = (`extd_intr == 1) ? ena_t1_1        : ena_t1_0;
  assign smod1         = (`extd_intr == 1) ? smod1_1         : smod1_0;


  //--------------------------
  // Implementation of Timer 2
  // (if desired)
  //--------------------------

// App Builder controls for conditional instantiation.
// reuse-pragma startSub Timer2_Instance [parameter_conditional_text %subText {@timer2 == 1}]
// reusepragma attr GenerateIf[0] {@timer2 == 1}
  DW8051_timer2 i_timer2
                (.clk             (clk),
                 .rst_n           (t_rst_out_n),
                 .sfr_addr        (int_sfr_addr),
                 .timer2_sfr_cs   (t_timer2_sfr_cs),
                 .timer2_data_out (t_timer2_data_out),
                 .timer2_data_in  (int_sfr_data_out),
                 .sfr_wr          (int_sfr_wr),
                 .cycle           (cpu_cycle),
                 .t2m             (t2m),
                 .t2              (t2),
                 .t2ex            (t2ex),
                 .t2_out          (t_t2_out),
                 .out_rclk        (t_rclk),
                 .out_tclk        (t_tclk),
                 .out_t2_ofl      (t_t2_ofl),
                 .out_tf2         (t_tf2),
                 .out_exf2        (t_exf2));
// reuse-pragma endSub Timer2_Instance

  assign  timer2_sfr_cs    = (`timer2 == 1) ? t_timer2_sfr_cs   : 0;
  assign  timer2_data_out  = (`timer2 == 1) ? t_timer2_data_out : 'b0;
  assign  rclk             = (`timer2 == 1) ? t_rclk   : 0; //defaults to timer 1
  assign  tclk             = (`timer2 == 1) ? t_tclk   : 0; //defaults to timer 1
  assign  t2_out           = (`timer2 == 1) ? t_t2_out : 0;
  assign  t2_ofl           = (`timer2 == 1) ? t_t2_ofl : 0;
  assign  tf2              = (`timer2 == 1) ? t_tf2    : 0;
  assign  exf2             = (`timer2 == 1) ? t_exf2   : 0;



  //----------------------------------
  // Implementation of standard serial
  // Interface (if desired)
  //----------------------------------
  // Serial port 0:

// App Builder controls for conditional instantiation.
// reuse-pragma startSub Serial0_Instance [parameter_conditional_text %subText {@serial > 0}]
// reusepragma attr GenerateIf[0] {@serial > 0}
  DW8051_serial #(`scon0_addr) i_serial1
                (.clk             (clk),
                 .rst_n           (t_rst_out_n),
                 .sfr_addr        (int_sfr_addr),
                 .serial_sfr_cs   (t_serial0_sfr_cs),
                 .serial_data_out (t_serial0_data_out),
                 .serial_data_in  (int_sfr_data_out),
                 .sfr_wr          (int_sfr_wr),
                 .t1_ofl          (t1_ofl),
                 .rclk            (rclk),
                 .tclk            (tclk),
                 .t2_ofl          (t2_ofl),
                 .cycle           (cpu_cycle),
                 .smod            (smod0),
                 .ri              (t_ri0),
                 .ti              (t_ti0),
                 .rxd_out         (t_rxd0_out),
                 .rxd_in          (rxd0_in),
                 .txd             (t_txd0));
// reuse-pragma endSub Serial0_Instance

  assign  serial0_sfr_cs   = (`serial > 0) ? t_serial0_sfr_cs   : 0;
  assign  serial0_data_out = (`serial > 0) ? t_serial0_data_out : 'b0;
  assign  ri0              = (`serial > 0) ? t_ri0      : 0;
  assign  ti0              = (`serial > 0) ? t_ti0      : 0;
  assign  rxd0_out         = (`serial > 0) ? t_rxd0_out : 1;
  assign  txd0             = (`serial > 0) ? t_txd0     : 1;

  // Serial port 1:
// App Builder controls for conditional instantiation.
// reuse-pragma startSub Serial1_Instance [parameter_conditional_text %subText {@serial == 2}]
// reusepragma attr GenerateIf[0] {@serial == 2}
  DW8051_serial	#(`scon1_addr) i_serial2
                (.clk             (clk),
                 .rst_n           (t_rst_out_n),
                 .sfr_addr        (int_sfr_addr),
                 .serial_sfr_cs   (t_serial1_sfr_cs),
                 .serial_data_out (t_serial1_data_out),
                 .serial_data_in  (int_sfr_data_out),
                 .sfr_wr          (int_sfr_wr),
                 .t1_ofl          (t1_ofl),
                 .rclk            (low),
                 .tclk            (low),
                 .t2_ofl          (low),
                 .cycle           (cpu_cycle),
                 .smod            (smod1),
                 .ri              (t_ri1),
                 .ti              (t_ti1),
                 .rxd_out         (t_rxd1_out),
                 .rxd_in          (rxd1_in),
                 .txd             (t_txd1));
// reuse-pragma endSub Serial1_Instance

  assign  serial1_sfr_cs   = (`serial == 2) ? t_serial1_sfr_cs   : 0;
  assign  serial1_data_out = (`serial == 2) ? t_serial1_data_out : 'b0;
  assign  ri1              = (`serial == 2) ? t_ri1      : 0;
  assign  ti1              = (`serial == 2) ? t_ti1      : 0;
  assign  rxd1_out         = (`serial == 2) ? t_rxd1_out : 1;
  assign  txd1             = (`serial == 2) ? t_txd1     : 1;



  //-------------------------
  // sfr_data_in multiplexer:
  //-------------------------
  assign int_sfr_data_in = (timer_sfr_cs   == 1) ? timer_data_out   :
                           (intr_sfr_cs    == 1) ? intr_data_out    :
                           (serial0_sfr_cs == 1) ? serial0_data_out :
                           (serial1_sfr_cs == 1) ? serial1_data_out :
                                                   timer2_data_out;

  assign  int_sfr_cs  = timer_sfr_cs   |
                        intr_sfr_cs    |
                        serial0_sfr_cs |
                        serial1_sfr_cs |
                        timer2_sfr_cs;


  // assignments for temporary signals:
  assign  rst_out_n    = t_rst_out_n;
  assign  mem_addr     = t_mem_addr;
  assign  mem_data_out = t_mem_data_out;
  assign  mem_psrd_n   = t_mem_psrd_n;


endmodule
