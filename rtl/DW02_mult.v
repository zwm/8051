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
// AUTHOR:    KB WSFDB		June 30, 1994
//
// VERSION:   Simulation Architecture
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------------
//
// ABSTRACT:  Multiplier
//           A_width-Bits * B_width-Bits => A_width+B_width Bits
//           Operands A and B can be either both signed (two's complement) or 
//	     both unsigned numbers. TC determines the coding of the input operands.
//           ie. TC = '1' => signed multiplication
//	         TC = '0' => unsigned multiplication
//
//	FIXED: by replacement with A tested working version
//		that not only doesn't multiplies right it does it
//		two times faster!
//
//------------------------------------------------------------------------------

module DW02_mult(A,B,TC,PRODUCT);
parameter	A_width = 8;
parameter	B_width = 8;
input	[A_width-1:0]	A;
input	[B_width-1:0]	B;
input			TC;
output	[A_width+B_width-1:0]	PRODUCT;

wire	[A_width+B_width-1:0]	PRODUCT;

wire	[A_width-1:0]	temp_a;
wire	[B_width-1:0]	temp_b;
wire	[A_width+B_width-2:0]	long_temp1,long_temp2;

assign	temp_a = (A[A_width-1])? (~A + 1'b1) : A;
assign	temp_b = (B[B_width-1])? (~B + 1'b1) : B;

assign	long_temp1 = temp_a * temp_b;
assign	long_temp2 = ~(long_temp1 - 1'b1);

assign	PRODUCT = (TC)? (((A[A_width-1] ^ B[B_width-1]) && (|long_temp1))?
				{1'b1,long_temp2} : {1'b0,long_temp1})
			: A * B;

endmodule


