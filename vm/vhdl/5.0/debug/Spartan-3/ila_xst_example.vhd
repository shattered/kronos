-------------------------------------------------------------------------------
-- Copyright (c) 1999-2005 Xilinx Inc.  All rights reserved.
-------------------------------------------------------------------------------
-- Title      : ILA Core Xilinx XST Usage Example
-- Project    : ChipScope
-------------------------------------------------------------------------------
-- File       : ila_xst_example.vhd
-- Company    : Xilinx Inc.
-- Created    : 2000/10/18
-------------------------------------------------------------------------------
-- Description: Example of how to instantiate the ILA core in a VHDL design
--              for use with the Xilinx XST synthesis tool.
-------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

entity ila_xst_example is
end ila_xst_example;

architecture structure of ila_xst_example is


  -------------------------------------------------------------------
  --
  --  ILA core component declaration
  --
  -------------------------------------------------------------------
  component ila
    port
    (
      control     : in    std_logic_vector(35 downto 0);
      clk         : in    std_logic;
      data        : in    std_logic_vector(31 downto 0);
      trig0       : in    std_logic_vector(7 downto 0)
    );
  end component;


  -------------------------------------------------------------------
  --
  --  ILA core signal declarations
  --
  -------------------------------------------------------------------
  signal control    : std_logic_vector(35 downto 0);
  signal clk        : std_logic;
  signal data       : std_logic_vector(31 downto 0);
  signal trig0      : std_logic_vector(7 downto 0);


begin


  -------------------------------------------------------------------
  --
  --  ILA core instance
  --
  -------------------------------------------------------------------
  i_ila : ila
    port map
    (
      control   => control,
      clk       => clk,
      data      => data,
      trig0     => trig0
    );


end structure;

