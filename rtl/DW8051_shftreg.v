// $Id: DW8051_shftreg.v,v 1.1 1996/07/25 17:43:25 gina Exp $
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
// FILE:     DW8051_shftreg.v
//
// AUTHOR:   Scott MacKay			May, 20, 1993
//
// VERSION:  entity DW8051_shftreg
//
// ABSTRACT: Universal Shift Register
//           length wordlength
//           shift enable active low
//           parallel load enable active low
// MODIFIED: 
//      L.Rieder        21.03.96        Adopted from DW03 to DW8051 environment;
//					async reset added
//      L.Rieder        05.06.96        Verilog version for fixed width of 10
//                                      created
//         
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//			May 20,98	Changed GTECH instantiations to HDL.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------


`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_shftreg    (clk,
                          s_in,
                          p_in,
                          shift_n,
                          load_n,
                          reset_n,
                          p_out
			  );
 
parameter width = 10;

 input clk;
 input s_in;
 input [(width-1):0] p_in;
 input shift_n;
 input load_n;
 input reset_n;
 output [(width - 1):0] p_out;
//------------------------------------------------------------------------------
wire [(width - 1):0] p_out;


//---------------
// local signals:
//---------------
reg [(width - 1):0] q;
wire [(width - 1):0] d;

//------------------------------------------------------------------------------

  assign d = (load_n == 0)  ? p_in :
	     (shift_n == 0) ? {q[(width-2):0],s_in} :
		              q; 

  always @(posedge clk or negedge reset_n)
    if (!reset_n)
      q <= 0;
    else
      q <= d;

  assign p_out = q;

endmodule
