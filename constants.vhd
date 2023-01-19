library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package constants is

type ALU_OP is 
    ( ALU_ADD, ALU_ADDW, ALU_SUB, ALU_SUBW, ALU_SLT, ALU_SLTU,
      ALU_XOR, ALU_OR, ALU_AND, -- Logical operators
      ALU_SLL, ALU_SLLW, ALU_SRL, ALU_SRLW, ALU_SRA, ALU_SRAW, -- Shifts
      ALU_MUL, ALU_MULW, ALU_MULH, ALU_MULHSU, ALU_MULHU, -- Multiplication
      ALU_DIV, ALU_DIVW, ALU_DIVU, ALU_DIVUW, ALU_REM, ALU_REMW, ALU_REMU, ALU_REMUW, ALU_NONE); -- Division

type FPU_OP is 
    ( FPU_ADD, FPU_SUB, FPU_MUL, FPU_DIV, FPU_SQRT,
      FPU_SGNJ, FPU_MINMAX, FPU_CMP, FPU_CLASS,
      FPU_CVT_FI, FPU_CVT_IF, FPU_CVT_FF, FPU_MV_FX, FPU_MV_XF,
      FPU_FMADD, FPU_FMSUB, FPU_FNMADD, FPU_FNMSUB, 
      FPU_NONE );

type MEM_OP is 
    ( LSU_LB, LSU_LBU, LSU_LH, LSU_LHU, LSU_LW, LSU_LWU, LSU_LD, 
      LSU_SB, LSU_SH, LSU_SW, LSU_SD, 
      LSU_FLW, LSU_FLD, LSU_FSW, LSU_FSD, 
      LSU_NONE );

constant EQ : STD_LOGIC_VECTOR (2 downto 0) := "000";
constant NE : STD_LOGIC_VECTOR (2 downto 0) := "001";
constant LT : STD_LOGIC_VECTOR (2 downto 0) := "100";
constant GE : STD_LOGIC_VECTOR (2 downto 0) := "101";
constant LTU : STD_LOGIC_VECTOR (2 downto 0) := "110";
constant GEU : STD_LOGIC_VECTOR (2 downto 0) := "111";

constant FLE : STD_LOGIC_VECTOR (2 downto 0) := "000"; 
constant FLT : STD_LOGIC_VECTOR (2 downto 0) := "001";
constant FEQ : STD_LOGIC_VECTOR (2 downto 0) := "010";

constant RESET_ADDRESS : STD_LOGIC_VECTOR (63 downto 0) := (others => '0');

constant LUI    : STD_LOGIC_VECTOR (6 downto 0) := "0110111";
constant AUIPC  : STD_LOGIC_VECTOR (6 downto 0) := "0010111"; 
constant JAL    : STD_LOGIC_VECTOR (6 downto 0) := "1101111"; 
constant JALR   : STD_LOGIC_VECTOR (6 downto 0) := "1100111";

constant BRANCH : STD_LOGIC_VECTOR (6 downto 0) := "1100011";
constant LOAD   : STD_LOGIC_VECTOR (6 downto 0) := "0000011";
constant STORE  : STD_LOGIC_VECTOR (6 downto 0) := "0100011"; 

constant LOAD_FP : STD_LOGIC_VECTOR (6 downto 0) := "0000111";
constant STORE_FP : STD_LOGIC_VECTOR (6 downto 0) := "0100111";
constant FP : STD_LOGIC_VECTOR (6 downto 0) := "1010011";
constant FMADD : STD_LOGIC_VECTOR (6 downto 0) := "1000011";
constant FMSUB : STD_LOGIC_VECTOR (6 downto 0) := "1000111"; 
constant FNMADD : STD_LOGIC_VECTOR (6 downto 0) := "1001011"; 
constant FNMSUB : STD_LOGIC_VECTOR (6 downto 0) := "1001111"; 
 
constant RI     : STD_LOGIC_VECTOR (6 downto 0) := "0010011";
constant RI32     : STD_LOGIC_VECTOR (6 downto 0) := "0011011";

constant RR     : STD_LOGIC_VECTOR (6 downto 0) := "0110011";
constant RR32   : STD_LOGIC_VECTOR (6 downto 0) := "0111011";

constant FENCE  : STD_LOGIC_VECTOR (6 downto 0) := "0001111";
constant SYSTEM : STD_LOGIC_VECTOR (6 downto 0) := "1110011";

constant FMV_XF : STD_LOGIC_VECTOR (4 downto 0) := "11100";
constant FMV_FX : STD_LOGIC_VECTOR (4 downto 0) := "11110";

constant INSTRUCTION_ADDRESS_MISALIGN : STD_LOGIC_VECTOR (3 downto 0) := X"0";
constant INSTRUCTION_ACCESS_FAULT : STD_LOGIC_VECTOR (3 downto 0) := X"1";
constant ILLEGAL_INSTRUCTION : STD_LOGIC_VECTOR (3 downto 0) := X"2";
constant BREAKPOINT : STD_LOGIC_VECTOR (3 downto 0) := X"3";
constant LOAD_ACCESS_FAULT : STD_LOGIC_VECTOR (3 downto 0) := X"5";
constant STORE_ACCESS_FAULT : STD_LOGIC_VECTOR (3 downto 0) := X"7";
constant ENVIROMENT_CALL_USER_MODE : STD_LOGIC_VECTOR (3 downto 0) := X"8";
constant NO_EXCEPTION : STD_LOGIC_VECTOR (3 downto 0) := X"F";

constant FFLAGS : STD_LOGIC_VECTOR (11 downto 0) := X"001"; 
constant FRM : STD_LOGIC_VECTOR (11 downto 0) := X"002";
constant FCSR : STD_LOGIC_VECTOR (11 downto 0) := X"003";

constant CYCLE : STD_LOGIC_VECTOR (11 downto 0) := X"C00";
constant TIMER : STD_LOGIC_VECTOR (11 downto 0) := X"C01";

constant MISA : STD_LOGIC_VECTOR (11 downto 0) := X"301";
constant MTVEC : STD_LOGIC_VECTOR (11 downto 0) := X"305";
constant MEPC : STD_LOGIC_VECTOR (11 downto 0) := X"341";
constant MCAUSE : STD_LOGIC_VECTOR (11 downto 0) := X"342";
constant MTVAL : STD_LOGIC_VECTOR (11 downto 0) := X"343";

constant CSR_NONE : STD_LOGIC_VECTOR (1 downto 0) := "00";
constant CSR_RW : STD_LOGIC_VECTOR (1 downto 0) := "01";
constant CSR_RS : STD_LOGIC_VECTOR (1 downto 0) := "10";
constant CSR_RC : STD_LOGIC_VECTOR (1 downto 0) := "11";
 
constant RNE : STD_LOGIC_VECTOR (2 downto 0) := "000";
constant RTZ : STD_LOGIC_VECTOR (2 downto 0) := "001";
constant RDN : STD_LOGIC_VECTOR (2 downto 0) := "010";
constant RUP : STD_LOGIC_VECTOR (2 downto 0) := "011";
constant RMM : STD_LOGIC_VECTOR (2 downto 0) := "100";
constant DYN : STD_LOGIC_VECTOR (2 downto 0) := "111";

constant FOUR : unsigned (63 downto 0) := to_unsigned(4, 64);

constant STRONGLY_NOT_TAKEN : STD_LOGIC_VECTOR (1 downto 0) := "00";
constant WEAKLY_NOT_TAKEN : STD_LOGIC_VECTOR (1 downto 0) := "01";
constant WEAKLY_TAKEN : STD_LOGIC_VECTOR (1 downto 0) := "10";
constant STRONGLY_TAKEN : STD_LOGIC_VECTOR (1 downto 0) := "11";

constant REFRESH_RATE : unsigned(19 downto 0) := to_unsigned(999999, 20);
constant NOP : STD_LOGIC_VECTOR (31 downto 0) := X"00000013";

constant MAX_VALUE : NATURAL := 868; -- ( 100 * 10^6 ) / 115200
constant HALF_MAX_VALUE : NATURAL := MAX_VALUE / 2;

--constant MAX_PC : unsigned (ADDRESS_WIDTH - 1 downto 0) := (ADDRESS_WIDTH-1 => '1', others => '0')

constant NEG_INFINITY : STD_LOGIC_VECTOR (9 downto 0) := (0 => '1', others => '0');
constant NEG_NORMAL : STD_LOGIC_VECTOR (9 downto 0) := (1 => '1', others => '0');
constant NEG_SUBNORMAL : STD_LOGIC_VECTOR (9 downto 0) := (2 => '1', others => '0');
constant NEG_ZERO : STD_LOGIC_VECTOR (9 downto 0) := (3 => '1', others => '0');
constant POS_ZERO : STD_LOGIC_VECTOR (9 downto 0) := (4 => '1', others => '0');
constant POS_SUBNORMAL : STD_LOGIC_VECTOR (9 downto 0) := (5 => '1', others => '0');
constant POS_NORMAL : STD_LOGIC_VECTOR (9 downto 0) := (6 => '1', others => '0');
constant POS_INFINITY : STD_LOGIC_VECTOR (9 downto 0) := (7 => '1', others => '0');
constant SIGNALING_NAN : STD_LOGIC_VECTOR (9 downto 0) := (8 => '1', others => '0');
constant QUIET_NAN : STD_LOGIC_VECTOR (9 downto 0) := (9 => '1', others => '0');

subtype word_t is STD_LOGIC_VECTOR(63 downto 0);
type reg_t is array(0 to 31) of word_t;

type FP_IDEX is record
    x : STD_LOGIC_VECTOR (63 downto 0);
    y : STD_LOGIC_VECTOR (63 downto 0);
    z : STD_LOGIC_VECTOR (63 downto 0);
    write : STD_LOGIC;
    precision : STD_LOGIC_VECTOR (1 downto 0);
    fp_op : FPU_OP;
    enable_fpu_subunit : STD_LOGIC_VECTOR (4 downto 0);
end record;

type REG is record
    write : STD_LOGIC;
    dest : STD_LOGIC_VECTOR (4 downto 0);
    data : STD_LOGIC_VECTOR (63 downto 0);
end record;

type CSR is record
    write : STD_LOGIC;
    write_addr : STD_LOGIC_VECTOR (11 downto 0);
    exception_id : STD_LOGIC_VECTOR (3 downto 0);
    epc : STD_LOGIC_VECTOR (63 downto 0);
    data : STD_LOGIC_VECTOR (63 downto 0);
end record;      

type MEMORY_REQUEST is record
    read : STD_LOGIC;
    write : STD_LOGIC;
    MDR : STD_LOGIC_VECTOR (63 downto 0);    
    MEMOp : MEM_OP;
end record;

type CACHE_REQUEST is record
    MAR : STD_LOGIC_VECTOR;
    MDR : STD_LOGIC_VECTOR (63 downto 0);
    we : STD_LOGIC_VECTOR (7 downto 0);
end record;

type BRANCH_INFO is record
    valid : STD_LOGIC;
    taken : STD_LOGIC;
    mispredict : STD_LOGIC;
    pc : STD_LOGIC_VECTOR;
    target_address : unsigned (63 downto 0);
end record;

type BRANCH_PREDICTION is record
    cf_type : STD_LOGIC_VECTOR (1 downto 0);
    predicted_address : unsigned (63 downto 0);
end record;

type FP_INFO is record
    normal : STD_LOGIC;
    subnormal : STD_LOGIC;
    zero : STD_LOGIC;
    inf : STD_LOGIC;
    nan : STD_LOGIC;
    signaling_nan : STD_LOGIC;
    quiet_nan : STD_LOGIC;
end record;

type FP_RESULT is record
    value : STD_LOGIC_VECTOR (63 downto 0); 
    fflags : STD_LOGIC_VECTOR (4 downto 0);
    valid : STD_LOGIC;
end record;

type FP_FORMAT is record
    P : NATURAL;
    E : NATURAL;
    M : NATURAL;
end record;

type fp_formats_array is array(0 to 1) of FP_FORMAT;
constant FP_FORMATS : fp_formats_array := (
    (32, 8, 24),
    (64, 11, 53)
);

type fp_infos_t is array (natural range <>) of FP_INFO;

function num_bits(size : natural) return natural;
function leading_zero_counter(X: unsigned; E : natural) return unsigned;
function leading_zero_counter(X : unsigned; E : natural) return signed;
function reverse(x : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
function check_rm(rm, frm : STD_LOGIC_VECTOR(2 downto 0)) return STD_LOGIC;
function fp_classify(fp_class : FP_INFO; sign : STD_LOGIC) return STD_LOGIC_VECTOR;
function fp_sign_injection(x : STD_LOGIC_VECTOR; x_sign : STD_LOGIC; y_sign : STD_LOGIC; funct3 : STD_LOGIC_VECTOR(2 downto 0)) return STD_LOGIC_VECTOR;
function leading_one_index(X : unsigned) return unsigned;
function q_length(x : natural) return natural;
function dst_fp_format(P : natural) return FP_FORMAT;
end package;

package body constants is

function num_bits(size : natural) return natural is
begin
    for i in 0 to 31 loop
        if 2**i >= size then
            return i;
        end if;
    end loop;
    return 0;
end function num_bits;

function leading_zero_counter(X : unsigned; E : natural) return unsigned is
    variable counter : unsigned(E-1 downto 0);
begin
    counter := (others => '0');
    for i in x'range loop
        case x(i) is
            when '0' => counter := counter + 1;
            when others => exit;
        end case;
    end loop;
    return counter;
end function;

function leading_zero_counter(X : unsigned; E : natural) return signed is
    variable counter : signed(E-1 downto 0);
begin
    counter := (others => '0');
    for i in x'range loop
        case x(i) is
            when '0' => counter := counter + 1;
            when others => exit;
        end case;
    end loop;
    return counter;
end function;


function reverse(x : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    variable output : STD_LOGIC_VECTOR(x'range);
begin
    for i in 0 to x'left loop
        output(x'left-i) := x(i);
    end loop;
    return output;
end function reverse;

function check_rm(rm, frm : STD_LOGIC_VECTOR(2 downto 0)) return STD_LOGIC is 
begin
    case rm is
        when "000" | "001" | "010" | "011" | "100" => return '0';
        when "111" => if unsigned(frm) > 4 then return '1'; end if;
        when others => return '1';
    end case;
    return '0'; 
end function check_rm;    

function fp_classify(fp_class : FP_INFO; sign : STD_LOGIC) return STD_LOGIC_VECTOR is
begin
    if fp_class.zero = '1' then
        if sign = '1' then return NEG_ZERO;
        else return POS_ZERO; end if;
    elsif fp_class.subnormal = '1' then
        if sign = '1' then return NEG_SUBNORMAL;
        else return POS_SUBNORMAL; end if;   
    elsif fp_class.normal = '1' then
        if sign = '1' then return NEG_NORMAL;
        else return POS_NORMAL; end if; 
    elsif fp_class.inf = '1' then
        if sign = '1' then return NEG_INFINITY;
        else return POS_INFINITY; end if;
    elsif fp_class.nan = '1' then
        if fp_class.signaling_nan = '1' then return SIGNALING_NAN;
        else return QUIET_NAN; end if;
    end if;
    return QUIET_NAN;
end function fp_classify;

function fp_sign_injection(x : STD_LOGIC_VECTOR; x_sign : STD_LOGIC; y_sign : STD_LOGIC; funct3 : STD_LOGIC_VECTOR(2 downto 0)) return STD_LOGIC_VECTOR is
    variable x_v : STD_LOGIC_VECTOR (x'length downto 0);
begin
    case funct3 is
        when "000" => x_v := y_sign & x;
        when "001" => x_v := not y_sign & x;
        when "010" => x_v := ( x_sign xor y_sign ) & x;
        when others => x_v := (others => '-');
    end case;
    return x_v;
end function fp_sign_injection;

function leading_one_index(X : unsigned) return unsigned is
    variable index : unsigned(5 downto 0);
begin

    index := (others => '0');
    for i in x'range loop
        if x(i) = '1' then
            return to_unsigned(i, index'length);
        end if;
    end loop;
    
    return index;
end function;

function q_length(x : natural) return natural is
begin
    if x mod 2 = 1 then
        return x+1;   
    end if;
    return x+2;
end function;

function dst_fp_format(P : natural) return FP_FORMAT is
begin
    if P = 32 then
        return FP_FORMATS(1);
    end if;
    return FP_FORMATS(0);
end function;

end package body;