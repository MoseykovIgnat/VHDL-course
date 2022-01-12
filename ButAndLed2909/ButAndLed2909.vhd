--Button and LEDS

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity ButAndLed2909 is
	port
	(
		clk_i :in std_logic;
		button_0_i : in std_logic;
		button_1_i : in std_logic;
		button_2_i : in std_logic;
		
		LED_0_o : out std_logic;
		LED_1_o : out std_logic;
		LED_2_o : out std_logic
	);
end entity;
architecture rtl of ButAndLed2909 is
	signal t0: std_logic;
	signal t1: std_logic;
	signal t3: std_logic;
	signal light_switch: std_logic;
begin
	process (clk_i)
	begin
			if(rising_edge(clk_i)) then
				if button_0_i = '1' then
					t0 <= button_0_i;
					t1 <= t0;
					t3 <= t0 and (not t1);
				end if;
				if t3 = '1' then
					light_switch <= (not light_switch);
				end if;
			end if;
	end process;
	
	LED_0_o <= light_switch;
	LED_1_o <= button_2_i;
	LED_2_o <= button_2_i;
	
end rtl;