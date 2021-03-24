// $Id: DW8051_timer_ctr.v,v 1.1 1996/07/25 17:43:40 gina Exp $
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
// FILE: DW8051_timer_ctr.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 timer counter module
//           (up with async reset, count_enable, load and
//           terminal count outputs) (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        05.06.96        Verilog version with fixed width
//					of 8 created
//	Bala Needamangalam May 20,1998	Changed the structural GTECH based
//					design to the RTL style, using HDL
//					statements.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------

`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_timer_ctr   (q,
                           q_n,
                           ones_all,
                           ones_5lsb,
                           clk,
                           rst_n,
                           ld_n,
                           data_in,
                           cnt_en
			   );
 
parameter width = 8;

 input clk;
 input rst_n;
 input ld_n;
 input [(width-1):0] data_in;
 input cnt_en;
 output [(width-1):0] q;
 output [(width-1):0] q_n;
 output ones_all;
 output ones_5lsb;

//------------------------------------------------------------------------------
wire [(width-1):0] q;
wire [(width-1):0] q_n;
wire ones_all;
wire ones_5lsb;

//--------------
//local signals:
//--------------
wire [(width-1):0] next_state;
reg  [(width-1):0] current_state;
wire [(width-1):0] max_count;
//------------------------------------------------------------------------------

 assign max_count = (1 << width) - 1;

 assign next_state = (ld_n == 0) ? data_in :
		     ((cnt_en == 0) ? current_state : current_state + 1);

 always @(posedge clk or negedge rst_n)
   if (!rst_n)
     current_state <= 0;
   else
     current_state <= next_state;

 assign q = current_state;
 assign q_n = ~current_state;
 assign ones_all = (current_state == max_count && cnt_en == 1) ? 1 : 0;
 assign ones_5lsb = (width < 5) ? 0 :
		    ((current_state[4:0] == 5'h1f && cnt_en == 1) ? 1 : 0);

endmodule
