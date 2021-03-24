// $Header: /am/remote/ped52/src/dware/ice/dev/dw/dw01/src/RCS/DW01_add.vhd,
// v 1.4 1994/07/15 10:31:40 scheidt v3-3-a-slot4 $
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
// ABSTRACT:  Adder
//
// MODIFIED: 
//
// Sheela    - May 10, 1995
// Converted from vhdl to verilog
// GN  Feb. 15th 1996: changed dw01 to DW01
//---------------------------------------------------------------------------

module DW01_add (A,B,CI,SUM,CO);

  parameter width=4;

  // port decalrations

  output [width-1 : 0]   SUM;
  output                 CO;
  input  [width-1 : 0]   A,B;
  input                  CI;

  reg    [width-1 : 0]   SUM;
  reg    [width-1 : 0]   sum_out;
  reg                    CO;
  reg                    c_out;

always 
  begin
    plus(sum_out,c_out,A,B,CI);
    SUM = sum_out;
    CO =  c_out;
  @(A or B or CI);
  end // process

  task plus;  
     output  [width-1 : 0] sumout;
     output                CO;
     input   [width-1 : 0] A,B;
     input                 CI;
     reg     [width-1 : 0] sumout,SUM;
     reg 	                 CO; 
     reg 	                 carry; 
     reg     [width-1 : 0] A,B;
     integer               i;

     begin
       carry = CI;
       for (i = 0; i <= width-1; i = i + 1) begin 
          sumout[i] = A[i] ^ B[i] ^ carry;
          carry = (A[i] & B[i]) | (A[i] & carry) | (carry & B[i]);
       end // loop
       SUM = sumout;
       CO = carry;
     end
  endtask // task 

  endmodule  // DW01_add;
