//-----------------------------------------------------------------------------
//
//       This confidential and proprietary software may be used only
//     as authorized by a licensing agreement from Synopsys Inc.
//     In the event of publication, the following notice is applicable:
//
//                    (C) COPYRIGHT 1994 - 2001   SYNOPSYS INC.
//                           ALL RIGHTS RESERVED
//
//       The entire notice above must be reproduced on all authorized
//     copies.
//
// AUTHOR:    Anatoly Sokhatsky
//
// VERSION:   Simulation Architecture
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------------
//
// ABSTRACT:  2-Function Comparator
//           When LEQ = 1   LT_LE: A =< B
//                          GE_GT: A > B
//           When LEQ = 0   LT_LE: A < B
//                          GE_GT: A >= B
//           When TC  = 0   Unsigned numbers
//           When TC  = 1   Two's - complement numbers
//
// MODIFIED: Sourabh  Dec. '98. 
//           Added functionality for X/Z handling.
//-------------------------------------------------------------------------------
module DW01_cmp2
  (A, B, LEQ, TC, LT_LE, GE_GT);

  parameter width = 8; 

  // port list declaration in order
  input [ width- 1: 0] A, B;
  input LEQ; // 1 => LEQ/GT 0=> LT/GEQ
  input TC; // 1 => 2's complement numbers
  output LT_LE;  reg LT_LE;
  output GE_GT;  reg GE_GT;

    function is_less;
    parameter sign = width - 1;
    input [width-1 : 0]  A, B;
    input TC; //Flag of Signed
    reg a_is_0, b_is_1, result ;
    integer i;
    begin
        if ( TC === 1'b0 ) begin  // unsigned numbers
          result = 0;
          for (i = 0; i <= sign; i = i + 1) begin
              a_is_0 = A[i] === 1'b0;
              b_is_1 = B[i] === 1'b1;
              result = (a_is_0 & b_is_1) |
                        (a_is_0 & result) |
                        (b_is_1 & result);
          end // loop
        end else begin  // signed numbers
          if ( A[sign] !== B[sign] ) begin
              result = A[sign] === 1'b1;
          end else begin
              result = 0;
              for (i = 0; i <= sign-1; i = i + 1) begin
                  a_is_0 = A[i] === 1'b0;
                  b_is_1 = B[i] === 1'b1;
                  result = (a_is_0 & b_is_1) |
                            (a_is_0 & result) |
                            (b_is_1 & result);
              end // loop
          end // if
        end // if
        is_less = result;
    end
    endfunction // 

  always
  begin : sim_mdl
   reg  ab_x, TC_int, LEQ_int;

   ab_x    = (^A)^(^B);
   TC_int  = TC | 1'b0;
   LEQ_int = LEQ | 1'b0;

   if ( TC_int === 1'bx || LEQ_int === 1'bx || ab_x === 1'bx ) begin
    GE_GT = 1'bx;
    LT_LE = 1'bx;
   end
   else begin
    if ( LEQ === 1'b1 ) begin
      if ( is_less(A,B,TC) || A === B ) begin
        GE_GT = 1'b0;
        LT_LE = 1'b1;
      end else begin
        GE_GT = 1'b1;
        LT_LE = 1'b0;
      end // if
    end else begin
      if ( is_less(B,A,TC) || A === B ) begin
        GE_GT = 1'b1;
        LT_LE = 1'b0;
      end else begin
        GE_GT = 1'b0;
        LT_LE = 1'b1;
      end // if
    end // if
   end
    @(A or B or LEQ or TC);
  end // process

endmodule // sim;

