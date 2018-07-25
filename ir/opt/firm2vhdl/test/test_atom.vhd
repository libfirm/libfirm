library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity test_atom is
  port(
    control : in  std_logic_vector(7 downto 0);
    clk     : in  std_logic;
    input0  : in  std_logic_vector(31 downto 0);
    input1  : in  std_logic_vector(31 downto 0);
    output0 : out std_logic_vector(31 downto 0);
    start   : in  std_logic;
    ready   : out std_logic
    );

  --attribute mult_style         : string;
  --attribute mult_style of Atom : entity is "lut";
  ---- alternative mult_styles are: {auto|block|lut|pipe_lut|CSD|KCM}
end test_atom;
