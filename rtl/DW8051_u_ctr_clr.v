// $Id: DW8051_u_ctr_clr_2.v,v 1.1 1996/07/25 17:43:45 gina Exp $
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
// FILE: DW8051_u_ctr_clr.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 counter module (up with sync.clear)
//
// MODIFICATION HISTORY:
//      L.Rieder        29.05.96        Verilog version for fixed width of 2
//                                      created
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//			May 20 98	Converted GTECH instantiations to HDL.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------
 

`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_u_ctr_clr   (q,
                           q_n,
                           clk,
                           clr_n,
                           rst_n
			   );
parameter width = 2;
 input  clk;
 input  clr_n;
 input  rst_n;
 output [(width-1):0] q;
 output [(width-1):0] q_n;

//------------------------------------------------------------------------------
wire clk;
wire clr_n;
wire rst_n;
reg [(width-1):0] q;
wire [(width-1):0] q_n;


//---------------
// local signals:
//---------------
wire [(width-1):0] next;

//------------------------------------------------------------------------------

    assign next = (clr_n == 1) ? (q + 1) : 0;

    always @(posedge clk or negedge rst_n)
      if (!rst_n)
	q <= 0;
      else 
	q <= next;

    assign q_n = ~q;

endmodule
