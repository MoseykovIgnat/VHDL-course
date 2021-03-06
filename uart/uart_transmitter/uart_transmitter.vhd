library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity uart_transmitter is
  generic (
    g_System_clock_speed : integer := 100000000; -- 100 ??? ???????
    g_Bit_Rate_Value : integer := 1000000; -- 1 ????/?
    -- g_Oversampling_Ratio : integer := 4; -- ??????? ????????????? ? 4 ???? ????, ??? ???????? ????????
    g_CLKS_PER_BIT : integer := 100 -- 100/1 ?? ??????? ??????    
    );
  port (
    clk_i       : in  std_logic;
    i_TX_DV     : in  std_logic;
    i_TX_Byte   : in  std_logic_vector(7 downto 0);
    o_TX_Active : out std_logic;
    o_TX_Serial : out std_logic;
    o_curr_clock: out std_logic;
    o_checker_parity: out std_logic;
    o_TX_Done   : out std_logic
    );
end uart_transmitter;
 
 
architecture RTL of uart_transmitter is

component Main_PLL
PORT
(
	inclk0		: IN STD_LOGIC  := '0';
	c0		: OUT STD_LOGIC 
);
end component; 
 
  type t_SM_Main is (s_Idle, s_TX_Start_Bit, s_TX_Data_Bits,
                     s_TX_parity_bit, s_TX_Stop_Bit, s_Cleanup);
  signal r_SM_Main : t_SM_Main := s_Idle;
 
  signal r_Clk_Count : integer range 0 to g_CLKS_PER_BIT-1 := 0;
  signal r_Bit_Index : integer range 0 to 7 := 0; 
  signal r_TX_Data   : std_logic_vector(7 downto 0) := (others => '0');
  signal r_TX_Parity : std_logic := '0';
  signal r_TX_Done   : std_logic := '0';
  signal CLK50_s : std_logic;
   
begin

---------------------------------------------------------PLL

	PLL: Main_PLL
		port map(
					inclk0 => clk_i,
					c0		=> CLK50_s
				);
	
---------------------------------------------------------END_PLL
  
  p_UART_TX : process (CLK50_s)
  begin
    if rising_edge(CLK50_s) then
        
      case r_SM_Main is

        when s_Idle =>
          o_TX_Active <= '0';
          o_TX_Serial <= '1';
          r_TX_Done   <= '0';
          r_Clk_Count <= 0;
          r_Bit_Index <= 0;

          if i_TX_DV = '1' then
            r_TX_Data <= i_TX_Byte;
            r_SM_Main <= s_TX_Start_Bit;        
          else
            r_SM_Main <= s_Idle;
          end if;

          
        -- ???? g_CLKS_PER_BIT-1 ?????? ??? ?????????? ???????? ???? ??????!
        when s_TX_Start_Bit =>
          o_TX_Active <= '1';
          o_TX_Serial <= '0';

          -- ???? g_CLKS_PER_BIT-1 ?????? ??? ?????????? ???????? ???? ??????!
          if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_TX_Start_Bit;
          else
            r_Clk_Count <= 0;
            r_SM_Main   <= s_TX_Data_Bits;
          end if;

          
        -- ???? g_CLKS_PER_BIT-1 ?????? ??? ???????? ??????? ???? ?? ???????????!         
        when s_TX_Data_Bits =>
          o_TX_Serial <= r_TX_Data(r_Bit_Index);
          
          if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_TX_Data_Bits;
          else
            r_Clk_Count <= 0;
            r_TX_Parity <= r_TX_Parity xor r_TX_Data(r_Bit_Index);
            -- ???????? ????????? ?? ??? ????
            if r_Bit_Index < 7 then
              r_Bit_Index <= r_Bit_Index + 1;
              r_SM_Main   <= s_TX_Data_Bits;
            else
              r_Bit_Index <= 0;
              r_SM_Main   <= s_TX_parity_bit;
            end if;
          end if;
		
		-- ?????????? CRC-1 (??? ???????? UART) r_TX_Parity ??????????
        when s_TX_parity_bit =>
          o_TX_Active <= '1';
          o_TX_Serial <= r_TX_Parity;

          -- ???? g_CLKS_PER_BIT-1 ?????? ??? ?????????? ???????? ???? ????????!
          if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_TX_parity_bit;
          else
            r_Clk_Count <= 0;
            r_SM_Main   <= s_TX_Stop_Bit;
          end if;

        -- ???????? ???? ???, ?????????? Stop bit = 1
        when s_TX_Stop_Bit =>
          o_TX_Serial <= '1';

          -- ???? g_CLKS_PER_BIT-1 ?????? ??? ?????????? ???????? ???? ?????!
          if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_TX_Stop_Bit;
          else
            r_TX_Done   <= '1';
            r_Clk_Count <= 0;
            r_SM_Main   <= s_Cleanup;
          end if;

                  
        -- Stay here 1 clock
        when s_Cleanup =>
          o_TX_Active <= '0';
          r_TX_Done   <= '1';
          r_SM_Main   <= s_Idle;
          
            
        when others =>
          r_SM_Main <= s_Idle;

      end case;
    end if;
  end process p_UART_TX;

  o_TX_Done <= r_TX_Done;
  o_curr_clock <= CLK50_s;
  o_checker_parity <= r_TX_Parity;
end RTL;