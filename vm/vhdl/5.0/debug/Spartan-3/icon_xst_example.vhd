-------------------------------------------------------------------------------
-- Copyright (c) 1999-2005 Xilinx Inc.  All rights reserved.
-------------------------------------------------------------------------------
-- Title      : ICON Core Xilinx XST Usage Example
-- Project    : ChipScope
-------------------------------------------------------------------------------
-- File       : icon_xst_example.vhd
-- Company    : Xilinx Inc.
-- Created    : 2002/03/27
-------------------------------------------------------------------------------
-- Description: Example of how to instantiate the ICON core in a VHDL 
--              design for use with the Xilinx XST synthesis tool.
-------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity icon_xst_example is
end icon_xst_example;

architecture structure of icon_xst_example is


  -------------------------------------------------------------------
  --
  --  ICON core component declaration
  --
  -------------------------------------------------------------------
  component icon
    port
    (
      control0    :   out std_logic_vector(35 downto 0)
    );
  end component;


  -------------------------------------------------------------------
  --
  --  ICON core signal declarations
  --
  -------------------------------------------------------------------
  signal control0       : std_logic_vector(35 downto 0);


begin

  
  -------------------------------------------------------------------
  --
  --  ICON core instance
  --
  -------------------------------------------------------------------
  i_icon : icon
    port map
    (
      control0    => control0
    );


end structure;

