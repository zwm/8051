// $Id: DW8051_updn_ctr.v,v 1.1 1996/07/25 17:43:48 gina Exp $
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
//       copies.                                                                
//
// FILE:     DW8051_updn_ctr.v
//
// AUTHOR:   PS			Feb. 10, 1993
//
// ABSTRACT: Synchronous Up/Down Counter
//           parameterizable wordlength (width > 0)
//	     clk	- positive edge-triggering clock
//           reset	- asynchronous reset (active low)
//           data	- data load input
//	     cen	- counter enable
//	     count	- counter state	
//
// MODIFIED: 
//      L.Rieder        21.03.96        Adopted from DW03 to DW8051 environment
//      L.Rieder        29.05.96        Verilog version for fixed width of 16
//                                      created
//          
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//			May 20,98	Converted GTECH instantiations to HDL.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_updn_ctr    (data,
                           up_dn,
                           load,
                           cen,
                           clk,
                           reset,
                           count,
                           tercnt
			   );
 parameter width = 16;
 
 input [(width-1):0] data;
 input up_dn;
 input load;
 input cen;
 input clk;
 input reset;
 output [(width-1):0] count;
 output tercnt;

//------------------------------------------------------------------------------
wire [(width-1):0] count;
wire tercnt;
wire [(width-1):0] max_count;

//---------------
// local signals:
//---------------
reg [(width-1):0] ctr_state;
wire [(width-1):0] next_state;
 
//------------------------------------------------------------------------------

  assign max_count = ( 1 << width) - 1;

  assign next_state = (load == 0) ? data : 
		      ((cen == 0) ? ctr_state :
		      ((up_dn == 1) ? (ctr_state + 1) : (ctr_state - 1)));
  
  always @(posedge clk or negedge reset)
    if (!reset)
      ctr_state <= 0;
    else
      ctr_state <= next_state;

  assign  count  = ctr_state;
  assign tercnt = ((up_dn == 1 && ctr_state == max_count) ||
		   (up_dn == 0 && ctr_state == 0)) ? 1 : 0;
endmodule
