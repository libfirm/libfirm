library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

library STD;
use STD.TEXTIO.all;

entity testbench is
end;

architecture testbench of testbench is
      component test_atom_ent is
        port(
          clk     : in  std_logic;
          param0  : in  std_logic_vector(7 downto 0);
          param1  : in  std_logic_vector(31 downto 0);
          param2  : in  std_logic_vector(31 downto 0);
          output0 : out std_logic_vector(31 downto 0);
          start   : in  std_logic;
          ready   : out std_logic);
      end component test_atom_ent;

      signal a_clk: std_logic := '0';
      signal a_control: std_logic_vector(7 downto 0) := (others=>'0');
      signal a_input0 : std_logic_vector(31 downto 0) := (others=>'0');
      signal a_input1 : std_logic_vector(31 downto 0) := (others=>'0');
      signal a_output0 : std_logic_vector(31 downto 0) := (others=>'0');
      signal a_start: std_logic := '0';
      signal a_ready: std_logic := '0';
begin
  testee: test_atom_ent port map(a_clk, a_control, a_input0, a_input1, a_output0, a_start, a_ready);

  process
  variable iline,oline : line;
  variable good : boolean;
  variable control : integer;
  variable input0 : integer;
  variable input1 : integer;
  begin
    while not endfile(INPUT) loop
      readline(INPUT, iline);
      read(iline, control, good);
      exit when not good;
      read(iline, input0, good);
      exit when not good;
      read(iline, input1, good);
      exit when not good;


      a_clk <= '0';
      -- apply signals
      wait for 1 ns;
      a_control <= std_logic_vector(to_signed(control, 8));
      a_input0 <=  std_logic_vector(to_signed(input0, 32));
      a_input1 <= std_logic_vector(to_signed(input1, 32));

      a_start <= '1';
      a_clk <= '1';
      wait for 1 ns;
      a_start <= '0';
      a_clk <= '0';
      wait for 1 ns;

      while a_ready /= '1' loop
        a_clk <= '1';
        wait for 1 ns;
        a_clk <= '0';
        wait for 1 ns;
      end loop;

      write(oline, to_integer(signed(a_output0)));
      writeline(OUTPUT, oline);
    end loop;
    wait;
    write(OUTPUT, "done");
  end process;
end testbench;
