-- project Timer base template
-- result timer
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use work.Math_PACK.all;

entity Timer is
	port 
	(
		clk_i	: in std_logic;
		
		SS_Pin 	: out SS_array(3 downto 0);
		
		cnt_sec_0x_o : out std_logic ;
		
		cur_sec_0x_o_val : out STD_LOGIC_VECTOR(3 DOWNTO 0);
		
		cnt_sec_x0_o : out std_logic ;
		
		cur_sec_x0_o_val : out STD_LOGIC_VECTOR(3 DOWNTO 0);
		
		cnt_min_0x_o : out std_logic ;
		
		cur_min_0x_o_val : out STD_LOGIC_VECTOR(3 DOWNTO 0);
		
		cnt_min_x0_o : out std_logic ;
		
		cur_min_x0_o_val : out STD_LOGIC_VECTOR(3 DOWNTO 0);
		
		button_1_1	: in std_logic;
		
		switch_1_1 : in std_logic;
		
		ref_freq_vp : out std_logic
	);
end entity;

architecture rtl of Timer is

	COMPONENT Seven_Segment
		PORT
		(
			clockasdfa		:	 IN STD_LOGIC;
			enable		:	 IN STD_LOGIC;
			Number		:	 IN STD_LOGIC_VECTOR(3 DOWNTO 0);
			Segment		:	 OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
		);
	END COMPONENT;
	
	component Main_PLL
	PORT
	(
		inclk0		: IN STD_LOGIC  := '0';
		c0		: OUT STD_LOGIC 
	);
	end component;
	
	CONSTANT Input_Freq : natural := 50000000; -- input freq
	CONSTANT PRESCALER_RATIO : natural := 2; -- step. For example 7. 50000000
	CONSTANT PRESCALER_SIZE : natural := f_log2(PRESCALER_RATIO); -- scaler
	
	type Number_array is array(natural range <>) of STD_LOGIC_VECTOR(3 DOWNTO 0);
	--- Massive for input dec digits
	signal Number_s	 :	 Number_array(3 DOWNTO 0);
	signal Segment_s :	 SS_array(3 downto 0);
	signal prescaler_s : STD_LOGIC_VECTOR(PRESCALER_SIZE -1 DOWNTO 0);
	--- Signals for increments
	signal Ref_Freq_s : std_logic;
	signal prescaler_increment_s : std_logic;
	--- increments variables
	signal cnt_sec_0x_s : STD_LOGIC_VECTOR(3 DOWNTO 0);
	signal cnt_sec_x0_s : STD_LOGIC_VECTOR(3 DOWNTO 0);
	signal cnt_min_0x_s : STD_LOGIC_VECTOR(3 DOWNTO 0);
	signal cnt_min_x0_s : STD_LOGIC_VECTOR(3 DOWNTO 0);
	signal cnt_sec_0x_carry_s : std_logic;
	signal cnt_sec_xx_carry_s : std_logic;
	signal cnt_min_0x_carry_s : std_logic;
	signal cnt_min_xx_carry_s : std_logic;
	signal CLK50_s : std_logic;
	--- increment button 
	signal btn_0_s : std_logic;
	signal btn_1_s : std_logic;
	signal btn_s : std_logic;
	--- increment method switch
	signal switch_s: std_logic:= '0';
begin
---------------------------------------------------------PLL

	PLL: Main_PLL
		port map(
					inclk0 => clk_i,
					c0		=> CLK50_s
				);
	
---------------------------------------------------------END_PLL


	process (CLK50_s)
	begin
		if (rising_edge(CLK50_s)) then
		    Number_s(0) <= cnt_sec_0x_s;
		    Number_s(1) <= cnt_sec_x0_s;
		    Number_s(2) <= cnt_min_0x_s;
		    Number_s(3) <= cnt_min_x0_s;
		end if;
	end process;
	
	chose_increment_method:process (CLK50_s)
	begin
		if (rising_edge(clk_i)) then
			switch_s <= switch_1_1;
		end if;
	end process;
	
	prescaler_for_timer:process (CLK50_s)
	begin
		if (rising_edge(clk_i)) then
			prescaler_s<=prescaler_s+'1';
			prescaler_increment_s <= '0';
			if (prescaler_s=std_logic_vector(to_unsigned(PRESCALER_RATIO - 1, PRESCALER_SIZE))) then
				prescaler_increment_s <= '1';
				prescaler_s <= (others => '0');
			end if;
				
		end if;
	end process;
	
	button_increment:process (CLK50_s)
	begin
		if (rising_edge(clk_i)) then
			btn_0_s <= not button_1_1;
			btn_1_s <= btn_0_s;
			btn_s <= btn_0_s and (not btn_1_s);
		end if;
	end process;
	
	Ref_Freq_s <= btn_s when switch_s='1' else prescaler_increment_s;
	
	sec_xx:process (CLK50_s)
	begin
		if (rising_edge(CLK50_s)) then
			if (Ref_Freq_s = '1') then
				cnt_sec_0x_s<=cnt_sec_0x_s+'1';
				cnt_sec_0x_carry_s <= '0';
				cnt_sec_xx_carry_s <= '0';
					if (cnt_sec_0x_s = 9) then
						cnt_sec_0x_carry_s <= '1';
						cnt_sec_0x_s<=(others => '0');
						cnt_sec_x0_s<=cnt_sec_x0_s+'1';
					end if;
					if (cnt_sec_x0_s = 5 and cnt_sec_0x_s = 9) then
						cnt_sec_x0_s<=(others => '0');
						cnt_sec_xx_carry_s <= '1';
						cnt_min_0x_s<=cnt_min_0x_s+'1';
					end if;
					if (cnt_sec_xx_carry_s='1') then
						cnt_min_0x_carry_s <= '0';
						cnt_min_xx_carry_s <= '0';
						if (cnt_min_0x_s = 9) then
							cnt_min_0x_carry_s <= '1';
							cnt_min_0x_s<=(others => '0');
							cnt_min_x0_s<=cnt_min_x0_s+'1';
							end if;
						if (cnt_min_x0_s = 5 and cnt_min_0x_s = 9) then
							cnt_min_x0_s<=(others => '0');
							cnt_min_xx_carry_s <= '1';
							end if;
					end if;
			end if;
		end if;
	end process;
	
--	min_xx:process (CLK50_s)
--	begin
--		if (rising_edge(CLK50_s)) then
--			if (Ref_Freq_s = '1' and cnt_sec_xx_carry_s='1') then
--				cnt_min_0x_s<=cnt_min_0x_s+'1';
--				cnt_min_0x_carry_s <= '0';
--				cnt_min_xx_carry_s <= '0';
--					if (cnt_min_0x_s = 9) then
--						cnt_min_0x_carry_s <= '1';
--						cnt_min_0x_s<=(others => '0');
--						cnt_min_x0_s<=cnt_min_x0_s+'1';
--						end if;
--					if (cnt_min_x0_s = 5 and cnt_min_0x_s = 9) then
--						cnt_min_x0_s<=(others => '0');
--						cnt_min_xx_carry_s <= '1';
--						end if;
--			end if;
--		end if;
--	end process;
	
	
	
	
	Indicator: For I in 0 to 3 generate
		SS_Display: Seven_Segment
			port map(
				clock	=> clk_i,
				enable  => '1',
				Number  => Number_s(I),
				Segment	=> Segment_s(I)
					);
					
		SS_Pin(I) <= Segment_s(I);
	End generate;
	
	ref_freq_vp <= CLK50_s;
	cnt_sec_0x_o <= cnt_sec_0x_carry_s;
	cur_sec_0x_o_val <= cnt_sec_0x_s;
	cnt_sec_x0_o <= cnt_sec_xx_carry_s;
	cur_sec_x0_o_val <= cnt_sec_x0_s;
	cnt_min_0x_o <= cnt_min_0x_carry_s;
	cur_min_0x_o_val <= cnt_min_0x_s;
	cnt_min_x0_o <= cnt_min_xx_carry_s;
	cur_min_x0_o_val <= cnt_min_x0_s;
	
end rtl;