-- g_CLKS_PER_BIT = (частота CLK50_s)/(частота UART)

-- 100 MHz Clock, 1_000_000 baud UART
-- (100_000_000)/(1_000_000) = 100
--
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
 
entity reciever is
  generic (
    g_System_clock_speed : integer := 100000000; -- 100 МГц частота
    g_Bit_Rate_Value : integer := 1000000; -- 1 Мбит/с
    g_Oversampling_Ratio : integer := 4; -- Частота сэмплирования в 4 раза выше, чем скорость передачи
    g_CLKS_PER_BIT : integer := 100 -- 100/(4*1) из строчек сверху
    );
  port (
    clk_i       : in  std_logic;
    
    o_status_idle		 		 : out std_logic;
    o_status_start_bit		 	 : out std_logic;
    o_status_rx_data_bits		 : out std_logic;
    o_status_rx_stop_bit		 : out std_logic;
    o_status_rx_clean_up		 : out std_logic;
    o_status_rx_parity_bit		 : out std_logic;
    
    i_RX_Serial : in  std_logic;
    o_RX_DV     : out std_logic;
    o_RX_Byte   : out std_logic_vector(7 downto 0);
    o_RX_parity_bit_from_serial : out std_logic;
    o_RX_parity_bit_calculated  : out std_logic;
    o_RX_is_parity_equel 		: out std_logic;
    o_RX_start_bit_accepted		: out std_logic;
    o_RX_stop_bit_accepted		: out std_logic;
    
    o_RX_curr_clock : out std_logic
    );
end reciever;
 
 
architecture rtl of reciever is
   
component Main_PLL
PORT
(
	inclk0		: IN STD_LOGIC  := '0';
	c0		: OUT STD_LOGIC 
);
end component; 
 
  type t_SM_Main is (s_Idle, s_RX_Start_Bit, s_RX_Data_Bits,
                     s_RX_parity_bit, s_RX_Stop_Bit, s_Cleanup);
  signal r_SM_Main : t_SM_Main := s_Idle;
 
  signal r_RX_Data_R : std_logic := '0';
  signal r_RX_Data   : std_logic := '0';
   
  signal r_Clk_Count : integer range 0 to g_CLKS_PER_BIT-1 := 0;
  signal r_Bit_Index : integer range 0 to 7 := 0;  -- 8 бит данных
  signal r_RX_Byte   : std_logic_vector(7 downto 0) := (others => '0');
  signal r_RX_DV     : std_logic := '0';
  signal r_RX_Parity_From_DV     : std_logic;
  signal r_RX_Calculated_Parity  : std_logic := '0';
  signal r_RX_is_parity_equel    : std_logic;
  signal r_RX_stop_bit_accepted  : std_logic:= '0';
  signal r_RX_start_bit_accepted : std_logic:= '0';
  
  signal r_status_idle		 		 : std_logic := '0';
  signal r_status_start_bit		 	 : std_logic := '0';
  signal r_status_rx_data_bits		 : std_logic := '0';
  signal r_status_rx_parity_bit		 : std_logic := '0';
  signal r_status_rx_stop_bit		 : std_logic := '0';
  signal r_status_rx_clean_up		 : std_logic := '0';
  
  signal CLK50_s : std_logic;
  
  

begin
---------------------------------------------------------PLL

	PLL: Main_PLL
		port map(
					inclk0 => clk_i,
					c0		=> CLK50_s
				);
	
---------------------------------------------------------END_PLL

 
  --- Уменьшение вероятности метастабильности
  p_SAMPLE : process (CLK50_s)
  begin
    if rising_edge(CLK50_s) then
      r_RX_Data_R <= i_RX_Serial;
      r_RX_Data   <= r_RX_Data_R;
    end if;
  end process p_SAMPLE;
  
  --- Отображаем какой из процессов идет в данный момент
  p_status: process (CLK50_s)
  begin
  case r_SM_Main is
	when s_Idle =>
	r_status_idle <= '1';
	r_status_start_bit <='0';
	r_status_rx_data_bits <= '0';
	r_status_rx_parity_bit <= '0';
	r_status_rx_stop_bit <= '0';
	r_status_rx_clean_up <= '0';
	
	when s_RX_Start_Bit =>
	r_status_idle <= '0';
	r_status_start_bit <='1';
	r_status_rx_data_bits <= '0';
	r_status_rx_parity_bit <= '0';
	r_status_rx_stop_bit <= '0';
	r_status_rx_clean_up <= '0';
	
	when s_RX_Data_Bits =>
	r_status_idle <= '0';
	r_status_start_bit <='0';
	r_status_rx_data_bits <= '1';
	r_status_rx_parity_bit <= '0';
	r_status_rx_stop_bit <= '0';
	r_status_rx_clean_up <= '0';
	
	when s_RX_parity_bit =>
	r_status_idle <= '0';
	r_status_start_bit <='0';
	r_status_rx_data_bits <= '0';
	r_status_rx_parity_bit <= '1';
	r_status_rx_stop_bit <= '0';
	r_status_rx_clean_up <= '0';
	
	when s_RX_Stop_Bit =>
	r_status_idle <= '0';
	r_status_start_bit <='0';
	r_status_rx_data_bits <= '0';
	r_status_rx_parity_bit <= '0';
	r_status_rx_stop_bit <= '1';
	r_status_rx_clean_up <= '0';
	
	when s_Cleanup =>
	r_status_idle <= '0';
	r_status_start_bit <='0';
	r_status_rx_data_bits <= '0';
	r_status_rx_parity_bit <= '0';
	r_status_rx_stop_bit <= '0';
	r_status_rx_clean_up <= '1';
	end case;
  end process p_status;

  p_UART_RX : process (CLK50_s)
  begin
    if rising_edge(CLK50_s) then
        
      case r_SM_Main is

        when s_Idle =>
          r_RX_DV     <= '0';
          r_Clk_Count <= 0;
          r_Bit_Index <= 0;
			-- Определили старт бит
          if r_RX_Data = '0' then       
            r_SM_Main <= s_RX_Start_Bit;
          else
            r_SM_Main <= s_Idle;
          end if;
          

		 
		-- Проверяем середину старт бита, если все еще низкий уровень - все супер, иначе перекидываем на IDLE
        when s_RX_Start_Bit =>
          	-- Ждем g_CLKS_PER_BIT-1 циклов для определения CRC-1
		 if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_RX_Start_Bit;
            if r_Clk_Count = (g_CLKS_PER_BIT-1)/2 then
				if r_RX_Data = '0' then
					r_SM_Main   <= s_RX_Start_Bit;
					r_RX_start_bit_accepted <= '1';
				else
					r_SM_Main   <= s_Idle;
					r_RX_start_bit_accepted <= '0';
				end if;	
			end if;
         else
			r_Clk_Count  <= 0;
			r_SM_Main   <= s_RX_Data_Bits;
		 end if;
          

          
        -- Получаем данные
        when s_RX_Data_Bits =>
			-- Ждем g_CLKS_PER_BIT-1 циклов для семплирования серии данных
          if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_RX_Data_Bits;
            -- Берем значение бита из середины
            if r_Clk_Count = (g_CLKS_PER_BIT-1)/2 then
				r_RX_Byte(r_Bit_Index) <= r_RX_Data;
				r_RX_Calculated_Parity <= r_RX_Calculated_Parity xor r_RX_Data;
			end if;
          else
            r_Clk_Count <= 0;
            -- Проверяем получули все битики или нет
            if r_Bit_Index < 7 then
              r_Bit_Index <= r_Bit_Index + 1;
              r_SM_Main   <= s_RX_Data_Bits;
            else
              r_Bit_Index <= 0;
              r_SM_Main   <= s_RX_parity_bit;
            end if;
          end if;
          
          
		-- Получаем бит четности
		when s_RX_parity_bit =>
			-- Ждем g_CLKS_PER_BIT-1 циклов для определения CRC-1
		 if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_RX_parity_bit;
         else
			r_Clk_Count  <= 0;
			r_RX_Parity_From_DV <= r_RX_Data;
			if r_RX_Parity_From_DV = r_RX_Calculated_Parity then
				r_RX_is_parity_equel <= '1';
			else
				r_RX_is_parity_equel <= '0';
			end if;
			r_SM_Main   <= s_RX_Stop_Bit;
		 end if;
		
        -- Получаем стоп бит
        when s_RX_Stop_Bit =>
          -- Ждем g_CLKS_PER_BIT-1 циклов для определения стоп бита
          if r_Clk_Count < g_CLKS_PER_BIT-1 then
            r_Clk_Count <= r_Clk_Count + 1;
            r_SM_Main   <= s_RX_Stop_Bit;
            if r_Clk_Count = (g_CLKS_PER_BIT-1)/2 then
				-- Проверяем середину стоп бита, если что-то пошло не так - чистим данные, выставляем IDLE
				if r_RX_Data = '1' then
					r_SM_Main   <= s_RX_Stop_Bit;
					r_RX_stop_bit_accepted <= '1';
				else
					r_SM_Main   <= s_Idle;
					r_RX_Byte <= (others => '0');
				end if;					
			end if;
          else
            r_RX_DV     <= '1';
            r_Clk_Count <= 0;
            r_SM_Main   <= s_Cleanup;
            r_RX_stop_bit_accepted <= '0';
          end if;

                  
        -- Чистка после приема
        when s_Cleanup =>
          r_SM_Main <= s_Idle;
          r_RX_DV   <= '0';
          r_RX_Parity_From_DV <= '0';
          r_RX_Calculated_Parity <= '0';
          r_RX_is_parity_equel <= '0';
          r_RX_Byte <= (others => '0');
          r_RX_start_bit_accepted <= '0';
          r_RX_stop_bit_accepted <= '0';

            
        when others =>
          r_SM_Main <= s_Idle;

      end case;
    end if;
  end process p_UART_RX;

  o_RX_DV   <= r_RX_DV;
  o_RX_Byte <= r_RX_Byte;
  o_RX_parity_bit_from_serial <= r_RX_Parity_From_DV;
  o_RX_parity_bit_calculated  <= r_RX_Calculated_Parity;
  o_RX_is_parity_equel <= r_RX_is_parity_equel;
  o_RX_start_bit_accepted <= r_RX_start_bit_accepted;
  o_RX_stop_bit_accepted <= r_RX_stop_bit_accepted;
  
  o_status_idle <= r_status_idle;
  o_status_start_bit <= r_status_start_bit;
  o_status_rx_data_bits <= r_status_rx_data_bits;
  o_status_rx_parity_bit <= r_status_rx_parity_bit;
  o_status_rx_stop_bit <= r_status_rx_stop_bit;
  o_status_rx_clean_up <= r_status_rx_clean_up;
  
  
  o_RX_curr_clock <= CLK50_s;
  
end rtl;