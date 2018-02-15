#!/usr/bin/env python3

import sys
import os.path

fin = open(sys.argv[1], 'rb')
fout = open(sys.argv[2], 'w')

# module name
modulename = "osd_bootstrap"
# output data size (bytes)
datasize = 2

data = fin.read()

array_len = int(len(data)/datasize)

# calculate addr size
adrsize = 0
fitsize = 1

while fitsize < array_len:
  adrsize += 1
  fitsize += fitsize

adrsize = 11

fout.write("""\
-- Preloaded RAM with single clock
-- converted by osdbootstrap_bin2vhdl.py

-- when pass_thru enabled on port
-- then Read-during-write on port should return newly written data

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity """ + modulename + """ is
	generic 
	(
		pass_thru_a: boolean := False;
		data_width: natural := """ + str(8*datasize) + """;
		addr_width: natural := """ + str(adrsize) + """
	);
	port 
	(
		clk: in std_logic;
		addr_a: in std_logic_vector((addr_width-1) downto 0);
		we_a: in std_logic_vector(1 downto 0) := "00"; -- write byte enable 
		data_in_a: in std_logic_vector((data_width-1) downto 0);
		data_out_a: out std_logic_vector((data_width -1) downto 0)
	);
end """ + modulename + """;

architecture rtl of """ + modulename + """ is
        signal R_data_out_a: std_logic_vector((data_width -1) downto 0);
	-- Build a 2-D array type for the RAM
	subtype byte_t is std_logic_vector(7 downto 0);
	type memory_t is array(0 to 2**addr_width-1) of byte_t;

	-- Declare the RAM
""")

last = array_len-1

fout.write("shared variable ram_l: memory_t := (");
if datasize == 2:
 for i in range(0, array_len):
  if (i % 8) == 0:
    fout.write("\n")
  fout.write("x\"%02x\"" % (data[2*i+1],) )
  if i != fitsize-1:
    fout.write(",")
if array_len < fitsize:
  fout.write("\nothers => (others => '0')");
fout.write(");\n")

fout.write("shared variable ram_h: memory_t := (");
if datasize == 2:
 for i in range(0, array_len):
  if (i % 8) == 0:
    fout.write("\n")
  fout.write("x\"%02x\"" % (data[2*i+0],) )
  if i != fitsize-1:
    fout.write(",")
if array_len < fitsize:
  fout.write("\nothers => (others => '0')");
fout.write(");\n")

fout.write("""
begin
	-- Port A
	G_port_a_passthru: if pass_thru_a generate
	process(clk)
	begin
	if(rising_edge(clk)) then
	    if(we_a(0) = '1') then
		ram_l(conv_integer(addr_a)) := data_in_a(7 downto 0);
	    end if;
	    if(we_a(1) = '1') then
		ram_h(conv_integer(addr_a)) := data_in_a(15 downto 8);
	    end if;
	    data_out_a <= ram_h(conv_integer(addr_a)) & ram_l(conv_integer(addr_a));
	end if;
	end process;
	end generate;

	G_port_a_not_passthru: if not pass_thru_a generate
	process(clk)
	begin
	if(rising_edge(clk)) then 
	    R_data_out_a <= ram_h(conv_integer(addr_a)) & ram_l(conv_integer(addr_a));
	    if(we_a(0) = '1') then
		ram_l(conv_integer(addr_a)) := data_in_a(7 downto 0);
	    end if;
	    if(we_a(1) = '1') then
		ram_h(conv_integer(addr_a)) := data_in_a(15 downto 8);
	    end if;
	end if;
	end process;
	data_out_a <= R_data_out_a;
	end generate;
end rtl;
""");

fin.close()
fout.close()

