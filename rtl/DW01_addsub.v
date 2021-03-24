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
// AUTHOR:    PS			Nov. 8, 1992
//
// VERSION:   Simulation Architecture
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------------
//
// ABSTRACT:  Adder-Subtractor
//           ADD_SUB= '1' : SUM <= A - B - CI
//           ADD_SUB= '0' : SUM <= A + B + CI
//           Carry-in and Carry-out is active high with both addition 
//           and subtraction.
// MODIFIED: 
//
// Sheela      -  May 11, 1995 
// Converted to verilog from vhdl
// GN changed dw01 to DW01 star 33068
//
//	12/02/98  	Bob Tong: STAR 59142
//	03/03/2000	Bob Tong: STAR 99907
//
//---------------------------------------------------------------------

module DW01_addsub (A,B,CI,ADD_SUB,SUM,CO);

  parameter width = 4;

  output  [width-1 : 0]     SUM;
  output                    CO;
  input   [width-1 : 0]     A, B;
  input                     CI, ADD_SUB;

  reg     [width-1 : 0]     SUM; 
  reg                       CO; 
  reg     [width-1 : 0]     sum_out;
  reg     [width-1 : 0]     diff;  
  reg                       c_out;
  wire    ADD_SUB_int;
      
  assign ADD_SUB_int = (ADD_SUB | 1'b0);

  always 
    begin
      if (ADD_SUB_int == 1'bX) 
       begin
         sum_out = {width {1'bX}};
         c_out = 1'bX;
         SUM <= sum_out;
       end
      else if (ADD_SUB_int == 1'b0) 
       begin
         plus(sum_out,c_out,A,B,CI);  
         SUM <= sum_out;
       end 
      else 
       begin
         subtract(diff,c_out,A,B,CI);
         SUM <= diff;
       end
      CO <= c_out;
      @(A or B or CI or ADD_SUB_int);
    end
  
  
  task plus;
      output   [width-1 : 0]   sum_out;
      output                   c_out;
      input    [width-1 : 0]   A,B; 
      input                    CI;
  
      reg      [width-1 : 0]   A,B;  
      reg 	               carry; 
      reg 	               c_out; 
      reg      [width-1 : 0]   sumout;
      integer                  i;
  
      begin
        carry = CI;
        for (i = 0; i <= width-1; i = i + 1) 
         begin 
           sumout[i] = A[i] ^ B[i] ^ carry;
           carry = (A[i] & B[i]) | (A[i] & carry) | (carry & B[i]);
         end
        sum_out = sumout;
        c_out = carry;
      end
  endtask 
  
  
  task subtract;
      output   [width-1 : 0]    diff;
      output                    CO; 
      input    [width-1 : 0]    A,B; 
      input                     CI;
  
      reg      [width-1 : 0]    diff;
      reg      [width-1 : 0]    bv;
      reg                       carry;
      reg                       CO;
      integer                   i;
  
      begin
        carry = ~CI;
        bv = ~ B;
        for (i = 0; i <= width-1; i = i + 1) 
         begin 
           diff[i] = A[i] ^ bv[i] ^ carry;
           carry = (A[i] & bv[i])|(A[i] & carry) | (carry & bv[i]);
         end
        CO = ~carry;
     end
  endtask 

endmodule
