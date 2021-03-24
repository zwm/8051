// $Header: /am/remote/ped52/src/dware/ice/dev/dw/DW01/src/RCS/DW01_sub.vhd,
//  v 1.5 1994/07/15 10:31:59 scheidt v3-3-a-slot4 $
//-----------------------------------------------------------------------------
//
//       This confidential and proprietary software may be used only
//     as authorized by a licensing agreement from Synopsys Inc.
//     In the event of publication, the following notice is applicable:
//
//                    (C) COPYRIGHT 1992 - 2001   SYNOPSYS INC.
//                           ALL RIGHTS RESERVED
//
//       The entire notice above must be reproduced on all authorized
//     copies.
//
// AUTHOR:    PS
//
// VERSION:   Simulation Architecture
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------------
//
// ABSTRACT:  Subtractor
//           Carry-In and Carry-Out are active high
//
// MODIFIED: 
//	     
// Sheela   - May 18, 1995
// GN Feb. 15, 1996 star 33068
//---------------------------------------------------------------------------

module DW01_sub (A, B, CI, DIFF, CO);

     parameter width=4;

     // port list declaration in order
     output   [ width-1 : 0]   DIFF;   
     output                    CO;   
     input    [ width-1 : 0]   A, B; 
     input                     CI; 

     reg      [ width-1 : 0]   DIFF;   
     reg                       CO;  
     reg  [width-1 : 0]        diff_out;
     reg                       c_out;

always begin
    subtract(diff_out,c_out,A,B,CI);
    DIFF = diff_out;
    CO = c_out;
  @(A or B or CI);
end // process

task subtract;
    output  [width-1 : 0]   DIFF; 
    output                  CO; 
    input   [width-1 : 0]   A,B; 
    input                   CI; 

    reg     [width-1 : 0]   DIFF;
    reg                     CO;
    reg     [width-1 : 0]   A,B;
    reg     [width-1 : 0]   bv;
    reg                     carry,carry1;
    integer                 i;

    begin
	carry = ~ CI;
	bv = ~ B;
	for (i = 0; i <= (width-1); i = i + 1) begin 
	    DIFF[i] = A[i] ^ bv[i] ^ carry;
	    carry = (A[i] & bv[i]) | (A[i] & carry) | (carry & bv[i]);
	end // loop
        CO = ~ carry;
    end
endtask // ;

endmodule // DW01_sub;
