// $Id: DW8051_package.inc,v 1.1 1996/07/22 16:58:18 rieder Exp $
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
// FILE: DW8051_package.inc
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 global package (Verilog version)
//
// MODIFICATION HISTORY:
//	L.Rieder	28.05.96	Verilog version created
//	Bala Needamangalam
//			Jan 28,1998	Changed all decimal addresses to
//					binary or hexadecimal format. This
//					is for VCS compatibility.
//					Values of scon0_addr and scon1_addr
//					have been changed to binary.
//------------------------------------------------------------------------------

//-----------------------------------------------------------------
// Global constants:
//-----------------------------------------------------------------
// special SFR addresses:
`define p0_addr      8'b10000000 // 80h
`define sp_addr      8'b10000001 // 81h
`define dpl_addr     8'b10000010 // 82h
`define dph_addr     8'b10000011 // 83h
`define dpl1_addr    8'b10000100 // 84h,new
`define dph1_addr    8'b10000101 // 85h,new
`define dps_addr     8'b10000110 // 86h,new
`define pcon_addr    8'b10000111 // 87h
`define tcon_addr    8'b10001000 // 88h
`define tmod_addr    8'b10001001 // 89h
`define tl0_addr     8'b10001010 // 8Ah
`define tl1_addr     8'b10001011 // 8Bh
`define th0_addr     8'b10001100 // 8Ch
`define th1_addr     8'b10001101 // 8Dh
`define ckcon_addr   8'b10001110 // 8Eh,new
`define spc_fnc_addr 8'b10001111 // 8Fh,new
`define p1_addr      8'b10010000 // 90h
`define exif_addr    8'b10010001 // 91h
`define mpage_addr   8'b10010010 // 92h,new
`define scon_addr    8'b10011000 // 98h
`define scon0_addr   152         // 98h
`define sbuf_addr    8'b10011001 // 99h
`define sbuf0_addr   8'b10011001 // 99h
`define p2_addr      8'b10100000 // A0h
`define ie_addr      8'b10101000 // A8h
`define p3_addr      8'b10110000 // B0h
`define ip_addr      8'b10111000 // B8h
`define scon1_addr   192         // C0h
`define sbuf1_addr   8'b11000001 // C1h
`define t2con_addr   8'b11001000 // C8h
`define rcap2l_addr  8'b11001010 // CAh
`define rcap2h_addr  8'b11001011 // CBh
`define tl2_addr     8'b11001100 // CCh
`define th2_addr     8'b11001101 // CDh
`define psw_addr     8'b11010000 // D0h
`define eicon_addr   8'b11011000 // D8h
`define acc_addr     8'b11100000 // E0h
`define eie_addr     8'b11101000 // E8h
`define b_addr       8'b11110000 // F0h
`define eip_addr     8'b11111000 // F8h


// cycle definitions:
`define c1 2'b00
`define c2 2'b01
`define c3 2'b10
`define c4 2'b11

`define s1p1 4'b0000
`define s1p2 4'b0001
`define s2p1 4'b0010
`define s2p2 4'b0011
`define s3p1 4'b0100
`define s3p2 4'b0101
`define s4p1 4'b0110
`define s4p2 4'b0111
`define s5p1 4'b1000
`define s5p2 4'b1001
`define s6p1 4'b1010
`define s6p2 4'b1011


// ALU operations:
`define alu_op_trans  6'b000000
`define alu_op_cpl    6'b000010
`define alu_op_da     6'b000100
`define alu_op_swap   6'b000110
`define alu_op_clr    6'b001000
`define alu_op_anl    6'b001010
`define alu_op_orl    6'b001100
`define alu_op_xrl    6'b001110
`define alu_op_inc    6'b010000
`define alu_op_dec    6'b010001
`define alu_op_cmp    6'b010010
`define alu_op_add    6'b010100
`define alu_op_addc   6'b010101
`define alu_op_sub    6'b010110
`define alu_op_subb   6'b010111
`define alu_op_rl     6'b011000
`define alu_op_rlc    6'b011001
`define alu_op_rr     6'b011010
`define alu_op_rrc    6'b011011
`define alu_op_mul    6'b011100
`define alu_op_div    6'b011110
`define alu_op_div_sr 6'b011111
`define alu_op_clrc   6'b100000
`define alu_op_clrb   6'b100001
`define alu_op_setbc  6'b100010
`define alu_op_setbb  6'b100011
`define alu_op_cplc   6'b100100
`define alu_op_cplb   6'b100101
`define alu_op_anlcb  6'b100110
`define alu_op_anlcbn 6'b100111
`define alu_op_orlcb  6'b101000
`define alu_op_orlcbn 6'b101001
`define alu_op_movcb  6'b101010
`define alu_op_movbc  6'b101011


