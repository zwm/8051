// $Id: DW8051_alu.v,v 1.1 1996/07/25 17:42:24 gina Exp $
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
// FILE: DW8051_alu.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 arithmetic logic unit (Verilog version)
//
// MODIFICATION HISTORY:
//	L.Rieder	28.05.96	Verilog version created
//
//      Gina Ngo        11.20.96        Fixed star 38722: added header
//	Bala Needamangalam
//			May 20,98	Converted GTECH instantiations to HDL.
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------

`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"


module DW8051_alu (clk,
                   a,			// a input
                   b,			// b input
                   c,			// c input
                   ci,			// carry in (CY)
                   aci,			// aux carry in (AC)
                   ovi,			// overflow in (CY)
                   res,			// result
                   c_res,		// result from input c
                   co,			// carry out (CY)
                   aco,			// aux carry out(AC)
                   ovo,			// overflow out (OV)
                   zero,		// result zero
                   equal,		// a=b
                   bit_sts,		// status of sel. bit
                   bit_pos,		// bit position (bit op)
                   alu_op               // alu operation
		   );		

 input clk;
 input [7:0] a;
 input [7:0] b;
 input [7:0] c;
 input ci;
 input aci;
 input ovi;
 output [7:0] res;
 output [7:0] c_res;
 output co;
 output aco;
 output ovo;
 output zero;
 output equal;
 output bit_sts;
 input [2:0] bit_pos;
 input [5:0] alu_op;

//------------------------------------------------------------------------------
//                             DESCRIPTION
//------------------------------------------------------------------------------
//
// alu_op codes:
//
// bit 5 4 3 2 1 0      Operation
// ------------------------------------------------
//     0 0 0 0 0 0      Transparent
//     0 0 0 0 0 1
//     0 0 0 0 1 0      CPL
//     0 0 0 0 1 1
//     0 0 0 1 0 0      DA
//     0 0 0 1 0 1
//     0 0 0 1 1 0      SWAP
//     0 0 0 1 1 1
//     0 0 1 0 0 0      CLR
//     0 0 1 0 0 1
//     0 0 1 0 1 0      ANL
//     0 0 1 0 1 1
//     0 0 1 1 0 0      ORL
//     0 0 1 1 0 1
//     0 0 1 1 1 0      XRL
//     0 0 1 1 1 1
//     0 1 0 0 0 0      INC
//     0 1 0 0 0 1      DEC
//     0 1 0 0 1 0      CMP
//     0 1 0 0 1 1
//     0 1 0 1 0 0      ADD
//     0 1 0 1 0 1      ADDC
//     0 1 0 1 1 0      SUB
//     0 1 0 1 1 1      SUBB
//     0 1 1 0 0 0      RL
//     0 1 1 0 0 1      RLC
//     0 1 1 0 1 0      RR
//     0 1 1 0 1 1      RRC
//     0 1 1 1 0 0      MUL (based on ADD)
//     0 1 1 1 0 1
//     0 1 1 1 1 0      DIV (based on SUB)
//     0 1 1 1 1 1      DIV shift right
//     --------------------
//     1 x 0 0 0 0      CLR C
//     1 x 0 0 0 1      CLR bit
//     1 x 0 0 1 0      SETB C
//     1 x 0 0 1 1      SETB bit
//     1 x 0 1 0 0      CPL C
//     1 x 0 1 0 1      CPL bit
//     1 x 0 1 1 0      ANL C,bit
//     1 x 0 1 1 1      ANL C,/bit
//     1 x 1 x 0 0      ORL C,bit
//     1 x 1 x 0 1      ORL C,/bit
//     1 x 1 x 1 0      MOV C,bit
//     1 x 1 x 1 1      MOV bit,C
//
//------------------------------------------------------------------------------

wire clk;
wire [7:0] a;
wire [7:0] b;
wire [7:0] c;
wire ci;
wire aci;
wire ovi;
wire [2:0] bit_pos;
wire [5:0] alu_op;
wire [7:0] res;
reg [7:0] c_res;
wire co;
wire aco;
wire ovo;
wire zero;
wire equal;
wire bit_sts;


//---------------
// local signals:
//---------------
wire alu_op0_n;
 
wire [7:0] cpl_res;
wire cpl_ac;
wire cpl_co;
wire cpl_ov;
 
wire [7:0] da_res;
wire da_ac;
wire da_co;
wire da_ov;
wire da_greater9_l;
wire da_add_06;
wire da_add_60;
wire da_greater9_h;
wire da_12,da_56;
wire [7:0] da_add_0;
wire [3:0] da_add_1;
wire [8:0] da_carry_0;
wire [4:0] da_carry_1;
wire [7:0] da_add_res_0;
wire [3:0] da_add_res_1;
 
wire [7:0] swap_res;
wire swap_ac;
wire swap_co;
wire swap_ov;
 
wire [7:0] clr_res;
wire clr_ac;
wire clr_co;
wire clr_ov;
 
wire [7:0] anl_res;
wire anl_ac;
wire anl_co;
wire anl_ov;
 
wire [7:0] orl_res;
wire orl_ac;
wire orl_co;
wire orl_ov;
 
wire [7:0] xrl_res;
wire xrl_ac;
wire xrl_co;
wire xrl_ov;
 
// wire for adder/subtractor (ADD,ADDC,SUBB):
wire as_ci;
wire as_carry0;
wire [8:0] asid_carry;
wire [7:0] as_a;			// a for add, not a for sub
wire [7:0] as_b;			// B for add, not B for sub
wire [7:0] asid_a;
wire [7:0] asid_b;
wire [7:0] asid_res;
wire as_ac;
wire as_co;
wire as_ov;
 
wire [7:0] aeqb;
wire [7:0] lt1;
wire [7:0] lt2;
wire [8:0] lt;
wire cmp_ac;
wire cmp_co;
wire cmp_ov;
 
wire [7:0] mul_res;
wire [7:0] mul_c_res;
wire mul_ac;
wire mul_co;
wire mul_ov_l;
wire mul_ov_h;
wire mul_ov;
 
wire [8:0] div_t_res;
wire [7:0] div_res;
reg div_res8;
wire div_res8_n;
wire [7:0] div_c_res;
wire div_ac;
wire div_co;
wire div_ov_l;
wire div_ov_h;
wire div_ov;
 
wire [7:0] rl_res;
wire rl_ac;
wire rl_co;
wire rl_ov;
 
wire [7:0] rr_res;
wire rr_ac;
wire rr_co;
wire rr_ov;
 
reg [7:0] lo_res;
reg [7:0] hi_res;
reg lo_ac;
reg hi_ac;
reg lo_co;
reg hi_co;
reg lo_ov;
reg hi_ov;
wire [7:0] arith_res;
wire arith_ac;
wire arith_ov;
wire arith_co;
 
wire bit_pos_0n;
wire bit_pos_1n;
wire bit_pos_2n;
wire [7:0] bit_mask;
reg sel_bit;				// selected bit
wire mod_bit;				// modified bit
reg mod_bit_l;
reg mod_bit_h;
wire bit_co;
reg bit_co_l;
reg bit_co_h;
wire [7:0] bit_res;			// result of bit op
 
wire clrc_co;
wire clrc_bit;
wire clrb_co;
wire clrb_bit;
wire setbc_co;
wire setbc_bit;
wire setbb_co;
wire setbb_bit;
wire cplc_co;
wire cplc_bit;
wire cplb_co;
wire cplb_bit;
wire anlcb_co;
wire anlcb_bit;
wire anlcbn_co;
wire anlcbn_bit;
wire orlcb_co;
wire orlcb_bit;
wire orlcbn_co;
wire orlcbn_bit;
wire movcb_co;
wire movcb_bit;
wire movbc_co;
wire movbc_bit;
 
wire [7:0] tmp_res;
 
wire [7:0] bit_co_vector_l;
wire [3:0] bit_co_vector_h;
wire [7:0] mod_bit_vector_l;
wire [3:0] mod_bit_vector_h;
wire [7:0] ac_vector_l;
wire [7:0] ac_vector_h;
wire [7:0] co_vector_l;
wire [7:0] co_vector_h;
wire [7:0] ov_vector_l;
wire [7:0] ov_vector_h;
//------------------------------------------------------------------------------

// alu_op inversions needed later:
assign alu_op0_n = ~alu_op[0];


//-----
// CPL:
//-----
  assign cpl_res = ~a;
  assign  cpl_ac  = aci; // no flags affected:
  assign  cpl_co  = ci;
  assign  cpl_ov  = ovi;



//----
// DA:
//----
		// Straight implementation of the DA instruction description
		// in the Intel 8051 User's Manual.
  assign  {da_carry_0[8],da_add_res_0[7:0]} = 
	     ((a[3:0] > 9) || (aci == 1)) ? (a[7:0] + 8'h06) :
			                     a[7:0];
  assign {da_carry_1[4],da_add_res_1[3:0]} = 
  	     ((ci == 1) || 
	      (da_add_res_0[7:4] > 9) ||
	      (da_carry_0[8] == 1)) ? (da_add_res_0[7:4] + 4'h6): 
				       da_add_res_0[7:4];

  assign  da_res  = {da_add_res_1[3:0], da_add_res_0[3:0]};

  // flags: co affected, ac,ov not affected:
  assign  da_ac  = aci;
  assign da_co = ci | da_carry_0[8] | da_carry_1[4] ;
  assign  da_ov  = ovi;



//------
// SWAP:
//------
  assign swap_res[3:0] = a[7:4];
  assign swap_res[7:4] = a[3:0];
  // no flags affected:
  assign  swap_ac  = aci;
  assign  swap_co  = ci;
  assign  swap_ov  = ovi;


//-----
// CLR:
//-----
  assign  clr_res  = 'b0;

  // no flags affected:
  assign  clr_ac  = aci;
  assign  clr_co  = ci;
  assign  clr_ov  = ovi;



//-----------
// ANL (AND):
//-----------
  assign anl_res = a & b;
  // no flags affected:
  assign  anl_ac  = aci;
  assign  anl_co  = ci;
  assign  anl_ov  = ovi;



//----------
// ORL (OR):
//----------
 assign orl_res = a | b;
  // no flags affected:
  assign  orl_ac  = aci;
  assign  orl_co  = ci;
  assign  orl_ov  = ovi;



//-----------
// XRL (XOR):
//-----------
  assign xrl_res = a ^ b;
  // no flags affected:
  assign  xrl_ac  = aci;
  assign  xrl_co  = ci;
  assign  xrl_ov  = ovi;


//-----------------------------------------
// ADD,ADDC,SUBB,INC and DEC is implemented
// as a mix of DW01_addsub(rpl) and
// DW01_incdec(rpl):
//-----------------------------------------
  // alu_op(0) determines if addition with/without carry:
  assign as_ci = ci & alu_op[0];

  // alu_op(1) determines if addition/subtraction:
  assign as_carry0 = as_ci ^ alu_op[1];

  // if (inc/dec) then asid_carry=1(inc), asid_carry=0(dec)
  // else asid_carry=as_carry
  assign asid_carry[0] = (alu_op[2]) ? as_carry0 : alu_op0_n; 

  // ci is active high
  // asid_carry is active low  inside carry chain for subtraction
  // asid_carry is active high inside carry chain for addition
  assign as_a = {alu_op[1],alu_op[1],alu_op[1],alu_op[1],
		 alu_op[1],alu_op[1],alu_op[1],alu_op[1]} ^ a ;
  assign asid_a = (alu_op[2]) ? as_a : a;
  assign asid_b = (alu_op[2] )? b : ({alu_op[0],alu_op[0], 
				      alu_op[0],alu_op[0], 
				      alu_op[0],alu_op[0], 
				      alu_op[0],alu_op[0]});
  assign {asid_carry[4],asid_res[3:0]} = asid_a[3:0] + asid_b[3:0] + 
					{3'b000,asid_carry[0]};
  assign {asid_carry[7],asid_res[6:4]} = asid_a[6:4] + asid_b[6:4] + 
					{2'b00,asid_carry[4]};
  assign {asid_carry[8],asid_res[7]} = asid_a[7] + asid_b[7] + asid_carry[7];


  // flags: ac, co and ov affected
  // carry out is always active high:
  assign as_ac = asid_carry[4] ^ alu_op[1];
  assign as_co = asid_carry[8] ^ alu_op[1];
  assign as_ov = asid_carry[7] ^ asid_carry[8];

//-----
// CMP:
//-----
//  assign lt[0] = 0;
  assign aeqb = ~(a ^ b);
//  assign lt1 = a | ~b ;
//  assign lt2 = ~(aeqb & lt);
//  assign lt[8:1] = ~(lt1[7:0] & lt2[7:0]);
  // flags: carry affected, ac,ov unchanged
  assign  cmp_ac  = aci;
//  assign  cmp_co  = lt [8];
  assign  cmp_co  = (a < b) ? 1 : 0;
  assign  cmp_ov  = ovi;




//-----
// MUL:
//-----
  // product (16 bit) is build of b & c:
  // if c(0) = 1 then shift right result of ADD,
  // else shift rigth input
  assign mul_res[6:0] = (c[0] == 0) ? b[7:1] : asid_res[7:1];
  // lower 8 bits:
  assign mul_c_res [6:0] = c[7:1];
  assign mul_res[7] = asid_carry[8] & c[0];
  assign mul_c_res[7] = (c[0] == 0) ? b[0] : asid_res[0];
  
  // flags: carry always cleared, ov affected, ac unchanged
  assign  mul_ac  = aci;
  assign  mul_co  = 0;
  // ov is set if result > 255
  assign mul_ov = mul_res[0] | mul_res[1] | mul_res[2] | mul_res[3] |
		  mul_res[4] | mul_res[5] | mul_res[6] | mul_res[7];



//-----
// DIV:
//-----
  // remainder (16 bit) is build of b&c:
  assign div_t_res[7:1] = (as_co == 0) ? asid_res[6:0] : b [6:0];
  assign div_c_res [7:1] = c[6:0];

  // save upper bit:
  assign div_t_res[8] = (as_co == 0) ? asid_res[7] : b[7];

  always @(posedge clk) 
    div_res8 <= div_t_res[8];
  assign div_res8_n = ~div_res8;

  assign  div_t_res [0] = c [7];

  assign div_c_res[0] = ~as_co;

  // if alu_op(0) = 1 then shift b right 1 bit:
  assign div_res[6:0] = (alu_op[0] == 0) ? div_t_res[6:0] : b[7:1];

  // msb bit is taken from saved bit:
  assign div_res[7] = (alu_op[0] == 0) ? div_t_res[7] : div_res8;


  // flags: carry always cleared, ov affected, ac unchanged
  assign  div_ac  = aci;
  assign  div_co  = 0;
  // ov is set if a=0:

  assign div_ov = ~((a[0] | a[1] | a[2] | a[3]) | (a[4] | a[5] | a[6] | a[7]));


//---------
// RL, RLC:
//---------
  // handle RL/RLC:
  // if (RLC) then rl_res(0)=ci else rl_res(0)=a(7)
  assign  rl_res [0] = (alu_op[0] == 0) ? a[7] : ci;
  assign  rl_res[7:1] = a[6:0];

  // flags: only co affected for RLC
  assign  rl_ac  = aci;
  // if (RLC) then rl_co=a(7)   else rl_co=ci
  assign rl_co = (alu_op[0] == 0) ? ci : a[7];
  assign  rl_ov  = ovi;



//---------
// RR, RRC:
//---------
  // handle RR/RRC:
  // if (RRC) then rr_res(7)=ci else rr_res(7)=a(0)
  assign  rr_res [7] = (alu_op[0] == 0) ? a[0] : ci;
  assign  rr_res [6:0] = a[7:1];

  // flags: only co affected for RRC
  assign  rr_ac  = aci;
  // if (RRC) then rr_co=a(0) else rr_co=ci
  assign rr_co = (alu_op[0] == 0) ? ci : a[0];
  assign  rr_ov  = ovi;



//***************************************************************************
//              B I T   O P E R A T I O N S
//***************************************************************************
 
  // bit pos decoder:
    assign bit_pos_0n = ~ bit_pos[0];
    assign bit_pos_1n = ~ bit_pos[1];
    assign bit_pos_2n = ~ bit_pos[2];
    assign bit_mask[0] = bit_pos_2n & bit_pos_1n & bit_pos_0n;
    assign bit_mask[1] = bit_pos_2n & bit_pos_1n & bit_pos[0];
    assign bit_mask[2] = bit_pos_2n & bit_pos[1] & bit_pos_0n;
    assign bit_mask[3] = bit_pos_2n & bit_pos[1] & bit_pos[0];
    assign bit_mask[4] = bit_pos[2] & bit_pos_1n & bit_pos_0n;
    assign bit_mask[5] = bit_pos[2] & bit_pos_1n & bit_pos[0];
    assign bit_mask[6] = bit_pos[2] & bit_pos[1] & bit_pos_0n;
    assign bit_mask[7] = bit_pos[2] & bit_pos[1] & bit_pos[0];

  // get bit to change:
  always @(bit_pos or a)
    case (bit_pos[2:0])	
      3'h0:  sel_bit <= a[0];
      3'h1:  sel_bit <= a[1];
      3'h2:  sel_bit <= a[2];
      3'h3:  sel_bit <= a[3];
      3'h4:  sel_bit <= a[4];
      3'h5:  sel_bit <= a[5];
      3'h6:  sel_bit <= a[6];
      3'h7:  sel_bit <= a[7];
      default: begin end
    endcase

  // change selected bit only:

  assign bit_res[7] = (bit_mask[7] == 0) ? a[7] : mod_bit;
  assign bit_res[6] = (bit_mask[6] == 0) ? a[6] : mod_bit;
  assign bit_res[5] = (bit_mask[5] == 0) ? a[5] : mod_bit;
  assign bit_res[4] = (bit_mask[4] == 0) ? a[4] : mod_bit;
  assign bit_res[3] = (bit_mask[3] == 0) ? a[3] : mod_bit;
  assign bit_res[2] = (bit_mask[2] == 0) ? a[2] : mod_bit;
  assign bit_res[1] = (bit_mask[1] == 0) ? a[1] : mod_bit;
  assign bit_res[0] = (bit_mask[0] == 0) ? a[0] : mod_bit;



//-------
// CLR C:
//-------
  assign  clrc_co  = 0;
  assign  clrc_bit = sel_bit;


//---------
// CLR bit:
//---------
   assign  clrb_co  = ci;
   assign  clrb_bit = 0;


//--------
// SETB C:
//--------
  assign  setbc_co  = 1;
  assign  setbc_bit = sel_bit;


//----------
// SETB bit:
//----------
  assign  setbb_co  = ci;
  assign  setbb_bit = 1;


//-------
// CPL C:
//-------
  assign cplc_co = ~ci;
  assign  cplc_bit  = sel_bit;


//---------
// CPL bit:
//---------
  assign  cplb_co  = ci;
  assign cplb_bit = ~sel_bit;


//-----------
// ANL C,bit:
//-----------
  assign anlcb_co = ci & sel_bit;
  assign  anlcb_bit  = sel_bit;


//-----------
// ANL C,/bit:
//-----------
  assign  anlcbn_co  = ci & ~sel_bit;
  assign  anlcbn_bit  = sel_bit;


//-----------
// ORL C,bit:
//-----------
  assign  orlcb_co = ci | sel_bit;
  assign  orlcb_bit  = sel_bit;


//------------
// ORL C,/bit:
//------------
  assign  orlcbn_co = ci | ~sel_bit;
  assign  orlcbn_bit  = sel_bit;


//-----------
// MOV C,bit:
//-----------
  assign  movcb_co  = sel_bit;
  assign  movcb_bit = sel_bit;


//-----------
// MOV bit,C:
//-----------
 assign  movbc_co  = ci;
 assign  movbc_bit = ci;



//***************************************************************************
//              O U T P U T     M U X
//***************************************************************************
 
  // Bit operations output mux:
  always @(alu_op or clrc_co or clrb_co or setbc_co or setbb_co or
		     cplc_co or cplb_co or anlcb_co or anlcbn_co) 
      case (alu_op[2:0])	
	3'h0: bit_co_l <= clrc_co;
	3'h1: bit_co_l <= clrb_co;
	3'h2: bit_co_l <= setbc_co;
	3'h3: bit_co_l <= setbb_co;
	3'h4: bit_co_l <= cplc_co;
	3'h5: bit_co_l <= cplb_co;
	3'h6: bit_co_l <= anlcb_co;
	3'h7: bit_co_l <= anlcbn_co;
        default: begin end
      endcase

  // higher is not fully decoded:
  always @(alu_op or orlcb_co or orlcbn_co or movcb_co or movbc_co)
      case (alu_op[1:0])	
	3'h0:  bit_co_h <= orlcb_co;
	3'h1:  bit_co_h <= orlcbn_co;
	3'h2:  bit_co_h <= movcb_co;
	3'h3:  bit_co_h <= movbc_co;
        default: begin end
      endcase

  assign bit_co = (alu_op[3] == 0) ? bit_co_l : bit_co_h;

  always @(alu_op or clrc_bit or clrb_bit or setbc_bit or setbb_bit or
		     cplc_bit or cplb_bit or anlcb_bit or anlcbn_bit) 
      case (alu_op[2:0])	
	3'h0: mod_bit_l <= clrc_bit;
	3'h1: mod_bit_l <= clrb_bit;
	3'h2: mod_bit_l <= setbc_bit;
	3'h3: mod_bit_l <= setbb_bit;
	3'h4: mod_bit_l <= cplc_bit;
	3'h5: mod_bit_l <= cplb_bit;
	3'h6: mod_bit_l <= anlcb_bit;
	3'h7: mod_bit_l <= anlcbn_bit;
        default: begin end
      endcase

  always @(alu_op or orlcb_bit or orlcbn_bit or movcb_bit or movbc_bit)
      case (alu_op[1:0])	
	3'h0:  mod_bit_h <= orlcb_bit;
	3'h1:  mod_bit_h <= orlcbn_bit;
	3'h2:  mod_bit_h <= movcb_bit;
	3'h3:  mod_bit_h <= movbc_bit;
        default: begin end
      endcase

  assign mod_bit = (alu_op[3] == 0) ? mod_bit_l : mod_bit_h;

//--------------
// muxes for res:
//--------------
  always @(alu_op or a or cpl_res or da_res or swap_res or clr_res or
		     anl_res or orl_res or xrl_res)
    case (alu_op[3:1])	
      3'h0: lo_res <= a;
      3'h1: lo_res <= cpl_res;
      3'h2: lo_res <= da_res;
      3'h3: lo_res <= swap_res;
      3'h4: lo_res <= clr_res;
      3'h5: lo_res <= anl_res;
      3'h6: lo_res <= orl_res;
      3'h7: lo_res <= xrl_res;
      default: begin end
    endcase

  always @(alu_op or asid_res or a or rl_res or rr_res or mul_res or div_res)
    case (alu_op[3:1])	
      3'h0: hi_res <= asid_res;
      3'h1: hi_res <= a;
      3'h2: hi_res <= asid_res;
      3'h3: hi_res <= asid_res;
      3'h4: hi_res <= rl_res;
      3'h5: hi_res <= rr_res;
      3'h6: hi_res <= mul_res;
      3'h7: hi_res <= div_res;
      default: begin end
    endcase

  assign arith_res = (alu_op[4] == 0) ? lo_res : hi_res;
  assign tmp_res = (alu_op[5] == 0) ? arith_res : bit_res;

  assign  res  = tmp_res;


//--------------
// muxes for ac
//--------------
  always @(alu_op or aci or cpl_ac or da_ac or swap_ac or clr_ac or
		     anl_ac or orl_ac or xrl_ac)
    case (alu_op[3:1])	
      3'h0: lo_ac <= aci;
      3'h1: lo_ac <= cpl_ac;
      3'h2: lo_ac <= da_ac;
      3'h3: lo_ac <= swap_ac;
      3'h4: lo_ac <= clr_ac;
      3'h5: lo_ac <= anl_ac;
      3'h6: lo_ac <= orl_ac;
      3'h7: lo_ac <= xrl_ac;
      default: begin end
    endcase

  always @(alu_op or aci or cmp_ac or as_ac or rl_ac or rr_ac or 
		     mul_ac or div_ac)
    case (alu_op[3:1])	
      3'h0: hi_ac <= aci;
      3'h1: hi_ac <= cmp_ac;
      3'h2: hi_ac <= as_ac;
      3'h3: hi_ac <= as_ac;
      3'h4: hi_ac <= rl_ac;
      3'h5: hi_ac <= rr_ac;
      3'h6: hi_ac <= mul_ac;
      3'h7: hi_ac <= div_ac;
      default: begin end
    endcase

  assign arith_ac = (alu_op[4] == 0) ? lo_ac : hi_ac;
  assign aco = (alu_op[5] == 0) ? arith_ac : aci;

//--------------
// muxes for co:
//--------------

  always @(alu_op or ci or cpl_co or da_co or swap_co or clr_co or
		     anl_co or orl_co or xrl_co)
    case (alu_op[3:1])	
      3'h0: lo_co <= ci;
      3'h1: lo_co <= cpl_co;
      3'h2: lo_co <= da_co;
      3'h3: lo_co <= swap_co;
      3'h4: lo_co <= clr_co;
      3'h5: lo_co <= anl_co;
      3'h6: lo_co <= orl_co;
      3'h7: lo_co <= xrl_co;
      default: begin end
    endcase

  always @(alu_op or ci or cmp_co or as_co or rl_co or rr_co or 
		     mul_co or div_co)
    case (alu_op[3:1])	
      3'h0: hi_co <= ci;
      3'h1: hi_co <= cmp_co;
      3'h2: hi_co <= as_co;
      3'h3: hi_co <= as_co;
      3'h4: hi_co <= rl_co;
      3'h5: hi_co <= rr_co;
      3'h6: hi_co <= mul_co;
      3'h7: hi_co <= div_co;
      default: begin end
    endcase


  assign arith_co = (alu_op[4] == 0) ? lo_co : hi_co;
  assign co = (alu_op[5] == 0) ? arith_co : bit_co;

//--------------
// muxes for ov:
//--------------

  always @(alu_op or ovi or cpl_ov or da_ov or swap_ov or clr_ov or
		     anl_ov or orl_ov or xrl_ov)
    case (alu_op[3:1])	
      3'h0: lo_ov <= ovi;
      3'h1: lo_ov <= cpl_ov;
      3'h2: lo_ov <= da_ov;
      3'h3: lo_ov <= swap_ov;
      3'h4: lo_ov <= clr_ov;
      3'h5: lo_ov <= anl_ov;
      3'h6: lo_ov <= orl_ov;
      3'h7: lo_ov <= xrl_ov;
      default: begin end
    endcase

  always @(alu_op or ovi or cmp_ov or as_ov or rl_ov or rr_ov or 
		     mul_ov or div_ov)
    case (alu_op[3:1])	
      3'h0: hi_ov <= ovi;
      3'h1: hi_ov <= cmp_ov;
      3'h2: hi_ov <= as_ov;
      3'h3: hi_ov <= as_ov;
      3'h4: hi_ov <= rl_ov;
      3'h5: hi_ov <= rr_ov;
      3'h6: hi_ov <= mul_ov;
      3'h7: hi_ov <= div_ov;
      default: begin end
    endcase

  assign arith_ov = (alu_op[4] == 0) ? lo_ov : hi_ov;
  assign ovo = (alu_op[5] == 0) ? arith_ov : ovi;


//---------------
// mux for c_res:
//---------------
  always @(alu_op or mul_c_res or div_c_res or c)
    case (alu_op[3:1])
      3'b110:  c_res <= mul_c_res;
      3'b111:  c_res <= div_c_res;
      default: c_res <= c;
    endcase

//----------------------
// Zero flag generation:
//----------------------

  assign zero = (tmp_res == 0) ? 1 : 0;


//-----------------------
// Equal flag generation:
//-----------------------

   assign equal = (aeqb == 8'hff) ? 1 : 0;


//-----------------------
// Bit status generation:
//-----------------------
  assign  bit_sts  = sel_bit;



endmodule
