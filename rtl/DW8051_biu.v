// $Id: DW8051_biu.v,v 1.2 1997/08/01 13:57:12 uhlander Exp $
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
// FILE: DW8051_biu.v
//
// AUTHOR: Ludwig Rieder
//
// ABSTRACT: DW8051 bus interface unit module (Verilog version)
//
// MODIFICATION HISTORY:
//      L.Rieder        28.05.96        Verilog version created
//      G.Uhlaender     30.07.97        signal ext_rom_access_2 added and used
//                                      in "ROM read case" to fix star 46708
//	(Bala Needamangalam incorporated this change on 08.21.97)
//	Bala Needamangalam Sep 5, 1997  Modified the expressions in the "ROM
//					address comparator statements. The 
//					original statements fail when the
//					rom_addr_size parameter is set to 16.
//
//                      July 20,1999    Removed all DesignWare-Foundation 
//                                      license checkout commands.
//------------------------------------------------------------------------------



`include "./DW8051/DW8051_package.inc"
`include "./DW8051/DW8051_parameter.v"

module DW8051_biu (// global signals:
                   clk,
                   rst_n,
                   cycle,		// act cycle
                   ea_n,
                   stop_mode_n,
                   idle_mode_n,

                   // signals to/from control:
                   md,			// memory delay
                   wrs,			// write select
                   rom_addr,		// program ptr
                   instr_reg,
                   ram_addr,
                   wr_ram_addr_h,
                   wr_ram_addr_l,
                   data_out,		// RAM/ROM data
                   wr_ram,		// req from ctrl
                   rd_ram,		// req from ctrl
                   rd_rom,		// req from ctrl
                   data_in,		// from mem
                   ram_access_rdy,

                   // external interface:
                   mem_addr,
                   mem_data_out,
                   mem_data_in,
                   mem_wr_n,
                   mem_rd_n,
                   mem_pswr_n,
                   mem_psrd_n,
                   mem_ale,
                   p0_mem_reg_n,
                   p0_addr_data_n,
                   p2_mem_reg_n,

                   // internal ROM interface:
                   int_rom_data_in,
                   int_rom_rd_n,
                   int_rom_cs_n
		   );


parameter rom_addr_size = 0;

 input clk;
 input rst_n;
 input [1:0]  cycle;
 input ea_n;
 input stop_mode_n;
 input idle_mode_n;
 input [2:0]  md;
 input wrs;
 input [15:0]  rom_addr;
 input [15:0]  ram_addr;
 input wr_ram_addr_h;
 input wr_ram_addr_l;
 input [7:0]  data_out;
 input wr_ram;
 input rd_ram;
 input rd_rom;
 input [7:0]  mem_data_in;
 input [7:0]  int_rom_data_in;
 output [7:0]  instr_reg;
 output [7:0]  data_in;
 output ram_access_rdy;
 output [15:0] mem_addr;
 output [7:0]  mem_data_out;
 output mem_wr_n;
 output mem_rd_n;
 output mem_pswr_n;
 output mem_psrd_n;
 output mem_ale;
 output p0_mem_reg_n;
 output p0_addr_data_n;
 output p2_mem_reg_n;
 output int_rom_rd_n;
 output int_rom_cs_n;

//------------------------------------------------------------------------------
//                             DESCRIPTION
//------------------------------------------------------------------------------
// The module supports normal instruction read, ROM read, RAM/ROM write and
// RAM read. If no special operation is requested, the normal operation is
// instruction read. The BIU will continue reading instructions (from supplied
// rom_addr, used in c1) until a special operation is recognized. This is done
// in c4. So requests have to occur in c1..c3.
// The operation after the execution of a special request is always a
// instruction read.
// The destination for a write is determined by the wrs (write select) input;
// '0' will perform a RAM write (mem_wr_n active), '1' will perform a ROM
// write (mem_pswr_n active).
//
//               |c1|c2|c3|c4|c1|c2|c3|c4|c1|c2|c3|c4|
// rom_addr (pc) xxxxxxxxxxxxxVVVxxxxxxxxxxxxxxxxxxxxx  must be valid in c1
// ram_addr      xxxxxxxxxxxxxxxxxxxVVVxxxxxxxxxxxxxxx  must be valid in c3
// wr_ram_h/l    xxxxxxxxxxxxxxxxxxx---xxxxxxxxxxxxxxx  must be set latest in c3
//
//------------------------------------------------------------------------------
 
wire clk;
wire rst_n;
wire [1:0] cycle;
wire ea_n;
wire stop_mode_n;
wire idle_mode_n;
wire [2:0] md;
wire wrs;
wire [15:0] rom_addr;
wire [15:0] ram_addr;
wire wr_ram_addr_h;
wire wr_ram_addr_l;
wire [7:0] data_out;
wire wr_ram;
wire rd_ram;
wire rd_rom;
wire [7:0] mem_data_in;
wire [7:0] int_rom_data_in;
reg  [7:0] instr_reg;
reg  [7:0] data_in;
reg  ram_access_rdy;
reg  [15:0] mem_addr;
reg  [7:0] mem_data_out;
reg  mem_wr_n;
reg  mem_rd_n;
reg  mem_pswr_n;
reg  mem_psrd_n;
wire mem_ale;
reg  p0_mem_reg_n;
reg  p0_addr_data_n;
reg  p2_mem_reg_n;
reg  int_rom_rd_n;
wire int_rom_cs_n;


//---------------
// local signals:
//---------------

//type bus_seq_type:
`define instr_rd_seq 2'b00
`define ram_seq      2'b01
`define rd_rom_seq   2'b10

reg  [1:0] bus_seq;
 
wire zero;
 
reg  ale_pos;			// ale latch running on pos.edge
reg  ale_neg;			// ale latch running on neg.edge
reg  start_ram_seq;
reg  ram_16bit_access;
 
// memory delay counter signals:
reg   md_ld_n;
reg   dec_md;
wire  md_end;
wire  md_zero;
wire  [2:0] md_count;
 
wire  ext_rom_access;		// internal/external ROM access
                                // if address provided by rom_addr
                                // signal

wire  ext_rom_access_2;         // internal/external ROM access
                                // if address provided by ram_addr
                                // signal
 
wire [15:0] max_introm_addr;

//------------------------------------------------------------------------------

  assign  zero  = 0;
  assign  int_rom_cs_n  = 0;

//  DW8051_updn_ctr_3 c0 (.data(md),
  DW8051_updn_ctr #(3) c0 (.data(md),
                        .up_dn(zero),
                        .load(md_ld_n),
                        .cen(dec_md),
                        .clk(clk),
                        .reset(rst_n),
                        .count(md_count),
                        .tercnt(md_end));

// check if md = "000";
  assign md_zero  = (md == 3'b000) ? 1 : 0;

//------------------------
// ROM address comparator:
//------------------------
  assign max_introm_addr  = (rom_addr_size == 0) ? 0 : 
			    ((17'b00000000000000001 << rom_addr_size)-1);

  assign ext_rom_access   = (rom_addr_size == 0) ? 1 : 
                           ((rom_addr <= max_introm_addr) & (ea_n == 1)) ? 0 : 1;

  assign ext_rom_access_2 = (rom_addr_size == 0) ? 1 : 
                           ((ram_addr <= max_introm_addr) & (ea_n == 1)) ? 0 : 1;


  always @ (posedge clk or negedge rst_n)
  begin: main_biu_proc_pos
    if (!rst_n)
    begin 
      mem_data_out     <= 'b0;
      mem_wr_n         <= 1;
      mem_rd_n         <= 1;
      mem_pswr_n       <= 1;
      mem_psrd_n       <= 1;
      ale_pos          <= 1;
      p0_mem_reg_n     <= 0;
      p0_addr_data_n   <= 0;
      p2_mem_reg_n     <= 0;
      mem_addr         <= 'b0;
      instr_reg        <= 'b0;
      data_in          <= 'b0;
      ram_access_rdy   <= 0;
      start_ram_seq    <= 0;
      bus_seq          <= `instr_rd_seq;
      ram_16bit_access <= 0;
      md_ld_n          <= 1;
      dec_md           <= 0;
      int_rom_rd_n     <= 1;
    end
    else
    begin
      if ((stop_mode_n == 0) | (idle_mode_n == 0))
      begin 
        //synchronous reset:
        mem_data_out     <= 'b0;
        mem_wr_n         <= 1;
        mem_rd_n         <= 1;
        mem_pswr_n       <= 1;
        mem_psrd_n       <= 1;
        ale_pos          <= 1;
        p0_mem_reg_n     <= 0;
        p0_addr_data_n   <= 0;
        if (ext_rom_access == 0) p2_mem_reg_n <= 0;
        instr_reg        <= 'b0;
        ram_access_rdy   <= 0;
        start_ram_seq    <= 0;
        bus_seq          <= `instr_rd_seq;
        ram_16bit_access <= 0;
        md_ld_n          <= 1;
        dec_md           <= 0;
        int_rom_rd_n     <= 1;
      end
      else
      begin
        if (wr_ram_addr_l == 1)			// _l sigal both for 8/16bit
        begin 
          md_ld_n  <= 0;			// load md counter
          if (wr_ram_addr_h == 1) ram_16bit_access <= 1;
          else                    ram_16bit_access <= 0;
        end 

        //-----------------------------------
        // process bus sequences:
        // (state decoder depending on cycle)
        //-----------------------------------
        case (bus_seq )

          //------------------
          // instruction read:
          //------------------
          `instr_rd_seq :
          begin
            case (cycle )
              `c1 : begin
                      if (ext_rom_access == 1)
                      begin 
                        p0_mem_reg_n   <= 1;		// p0 output is address
                        p0_addr_data_n <= 1;
                        p2_mem_reg_n   <= 1;		// for whole sequence
                        ale_pos        <= 0;		// start ale cycle
                      end
                      else
                      begin
                        p0_mem_reg_n <= 0;		// release port 0
                        p2_mem_reg_n <= 0;		// release port 2
                      end 
                      mem_addr  <= rom_addr;
                    end
              `c2 : begin
                      p0_addr_data_n <= 0;		// next read/write data
                      p0_mem_reg_n   <= 0;		// disable output driver
							// (port holds 0FFh)
                      if (ext_rom_access == 1)
                        mem_psrd_n   <= 0;		// start ext.ROM read
                      else
                        int_rom_rd_n <= 0;		// start int.ROM read
                    end
              `c3 : begin
                    end
              `c4 : begin
                      if (ext_rom_access == 1)
                           instr_reg  <= mem_data_in;
                      else instr_reg  <= int_rom_data_in;
                      ale_pos      <= 1;		// finish ale cycle
                      mem_psrd_n   <= 1;		// finish ext.ROM read
                      int_rom_rd_n <= 1;		// finish int.ROM read

                      // next bus sequence ram access ?
                      if (wr_ram == 1)
                      begin 
                        bus_seq       <= `ram_seq;
                        start_ram_seq <= 1;
                      end
                      else if (rd_ram == 1)
                      begin 
                        bus_seq       <= `ram_seq;
                        start_ram_seq <= 1;
                      end
                      else if (rd_rom == 1)
                           bus_seq <= `rd_rom_seq;
                      else bus_seq <= `instr_rd_seq;
                    end
           default: begin
                    end
            endcase
          end  //instr_rd_seq

          //------------------------
          // RAM read,RAM/ROM/write:
          //------------------------
          `ram_seq :
          begin
            case (cycle )
              `c1 : begin
                      if (start_ram_seq == 1)
                      begin 
                        start_ram_seq  <= 0;
                        p0_mem_reg_n   <= 1;		// p0 output is address
                        p0_addr_data_n <= 1;
                        if (ram_16bit_access == 1)
                             p2_mem_reg_n <= 1;		// for whole sequence
                        else p2_mem_reg_n <= 0;		// for whole sequence
                      end 
                      ale_pos      <= 0;		// start ale cycle
                      md_ld_n      <= 1;		// finish md load
                      dec_md       <= 0;
                      mem_addr     <= ram_addr;
                      mem_data_out <= data_out;		// in any way
                    end
              `c2 : begin
                      p0_addr_data_n <= 0;		// next cycle rd/wr data
                      if (rd_ram == 1)			// if ram read,
                        p0_mem_reg_n  <= 0;		// disable p0 output drv
                      if (md_zero == 1)
                      begin 
                        if (rd_ram == 1)
                          mem_rd_n  <= 0;		// start ram read
                        else
                        begin
                          if (wrs == 0)
                            mem_wr_n   <= 0;		// start ram write
                          else
                            mem_pswr_n <= 0;		// start rom write
                        end 
                      end 
                      if (md_end == 1)
                        ram_access_rdy <= 1;
                    end
              `c3 : begin
                      if (md_zero == 0)
                      begin 
                        if (md_end == 0)
                        begin 
                          if (rd_ram == 1)
                            mem_rd_n  <= 0;		// start ram read
                          else
                          begin
                            if (wrs == 0)
                              mem_wr_n   <= 0;		// start ram write
                            else
                              mem_pswr_n <= 0;		// start rom write
                          end 
                        end
                        else
                        begin				// md cycles done,
                          mem_wr_n   <= 1;		// finish RAM wr cycle
                          mem_pswr_n <= 1;		// finish ROM wr cycle
                          mem_rd_n   <= 1;		// finish RAM rd cycle
                          if (rd_ram == 1)
                            data_in  <= mem_data_in;
                        end 
                      end 
                    end
              `c4 : begin
                      if (md_end == 1)
                      begin 
                        bus_seq    <= `instr_rd_seq;	// next sequence: instr.
                        ale_pos    <= 1;		// finish ale cycle
                        mem_wr_n   <= 1;		// for md = 0
                        mem_pswr_n <= 1;		// for md = 0
                        mem_rd_n   <= 1;		// for md = 0
                        if ((md_zero == 1) & (rd_ram == 1))
                          data_in  <= mem_data_in;
                        ram_access_rdy  <= 0;
                      end
                      else
                        dec_md  <= 1;			// decrement md counter
                    end
           default: begin
                    end
            endcase  // cycle
          end  // ram_seq

          //----------
          // ROM read:
          //----------
          `rd_rom_seq :
          begin
            case (cycle )
              `c1 : begin
                      if (ext_rom_access_2 == 1)
                      begin 
                        p0_mem_reg_n   <= 1;		// p0 output is address
                        p0_addr_data_n <= 1;
                        p2_mem_reg_n   <= 1;		// for whole sequence
                        ale_pos        <= 0;		// start ale cycle
                      end
                      else
                      begin
                        p0_mem_reg_n <= 0;		// release port 0
                        p2_mem_reg_n <= 0;		// release port 2
                      end 
                      mem_addr <= ram_addr;		// !!
                    end
              `c2 : begin
                      p0_addr_data_n <= 0;		// next read/write data
                      p0_mem_reg_n   <= 0;		// disable output driver
							// (port holds 0FFh)
                      if (ext_rom_access_2 == 1)
                        mem_psrd_n   <= 0;		// start ext.ROM read
                      else
                        int_rom_rd_n <= 0;		// start int.ROM read
                    end
              `c4 : begin
                      if (ext_rom_access_2 == 1)
                        data_in  <= mem_data_in;	// get ext.ROM data
                      else
                        data_in  <= int_rom_data_in;	// get int.ROM data
                      ale_pos      <= 1;		// finish ale cycle
                      mem_psrd_n   <= 1;		// finish ext.ROM read
                      int_rom_rd_n <= 1;		// finish int.ROM read
                      bus_seq      <= `instr_rd_seq;	// back to normal
                    end
           default: begin
                    end
            endcase  // cycle
          end  // rd_rom_seq

          default: begin
                   end
        endcase  // bus_seq
      end 
    end  //posedge clk
  end  //main_biu_proc_pos


  always @(negedge clk)
  begin : main_biu_proc_neg
    if (rst_n == 0)
      ale_neg  <= 1;
    else
      ale_neg  <= ale_pos;
  end 

// mem_ale generation (1.5 clock cycles high):
  assign  mem_ale = (ale_pos | ale_neg);


endmodule
