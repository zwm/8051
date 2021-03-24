// $Id: DW8051_core.v,v 1.1 1996/07/25 17:42:36 gina Exp $
//------------------------------------------------------------------------------
//
//        This confidential and proprietary software may be used only
//     as authorized by a licensing agreement from Synopsys Inc.
//     In the event of publication, the following notice is applicable:
//
//                    (C) COPYRIGHT 1999   SYNOPSYS INC.
//                          ALL RIGHTS RESERVED
//
//        The entire notice above must be reproduced on all authorized
//        copies.
//
// FILE: DW8051_parameter.v
//
// AUTHOR: Bala Needamangalam
//
// ABSTRACT: DW8051 Parameter Definitions.
//
// MODIFICATION HISTORY:
//----------------------------------------------------------------------------
//          Date             Modified By                Description
//----------------------------------------------------------------------------
//      Jan 25, 1998       Bala Needamangalam         Definitions of the 
//                                                    configurable parameters
//                                                    of the DW8051_core.
//      Jul 20, 1999       Bala Needamangalam         Changed "Enhanced" to 
//                                                    "Extended" in the 
//                                                    description attribute of
//                                                    extd_intr.
//----------------------------------------------------------------------------


// reuse-pragma beginAttr Description
// The amount of Internal (scratchpad) RAM to be configured.
// Valid sizes are: 128 bytes and 256 bytes.
// reuse-pragma endAttr
// reuse-pragma attr EnumValues 0 1
// reuse-pragma attr SymbolicNames {128 bytes} {256 bytes}
// reuse-pragma attr Label Size of Internal RAM
// reuse-pragma startSub [ReplaceConstantParam %subText]
`define ram_256 1

// reuse-pragma attr Description Should Timer 2 be included in the core ?
// reuse-pragma attr EnumValues 0 1
// reuse-pragma attr Label Timer 2
// reuse-pragma attr SymbolicNames {Removed} {Included}
// reuse-pragma startSub [ReplaceConstantParam %subText]
`define timer2 1

// reuse-pragma beginAttr Description
// The width of the Internal ROM address bus. Valid values are from 0 to 16 bits.
// Selecting 0 means no Internal ROM is present.
// reuse-pragma endAttr
// reuse-pragma attr MinValue 0
// reuse-pragma attr MaxValue 16
// reuse-pragma attr Label Internal ROM Address Bus Width (bits)
// reuse-pragma startSub [ReplaceConstantParam %subText]
`define rom_addr_size 16

// reuse-pragma beginAttr Description
// The number of serial ports required in the core.
// Valid values are: None, One and Two.
// reuse-pragma endAttr
// reuse-pragma attr EnumValues 0 1 2
// reuse-pragma attr Label Number of Serial Ports
// reuse-pragma attr SymbolicNames None One Two
// reuse-pragma startSub [ReplaceConstantParam %subText]
`define serial 2

// reuse-pragma beginAttr Description
// The Interrupt Unit used in the core.
// Valid values are: Standard and Extended.
// reuse-pragma endAttr
// reuse-pragma attr EnumValues 0 1
// reuse-pragma attr Label Interrupt Unit Type
// reuse-pragma attr SymbolicNames {Standard} {Extended}
// reuse-pragma startSub [ReplaceConstantParam %subText]
`define extd_intr 1

