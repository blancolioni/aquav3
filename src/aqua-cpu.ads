with Aqua.Addressable;

package Aqua.CPU is

   type Instance
     (Addressable : access Aqua.Addressable.Instance'Class)
   is tagged limited private;

   type Reference is access all Instance'Class;

   procedure Start (This             : in out Instance'Class;
                    Initial_Location : Address_Type);

private

   Register_Count : constant := 256;

   type Register_Index is new Word_8;

   Register_Window_Size : constant := 512;
   type Window_Register_Index is mod Register_Window_Size;

   type Window_Register_Array is array (Window_Register_Index) of Word;

   subtype System_Global is Register_Index range 0 .. 31;
   subtype General_Global is Register_Index range 32 .. 255;

   type Global_Register_Array is array (General_Global) of Word;

   G_Bootstrap      : constant System_Global := 0;
   G_Dividend       : constant System_Global := 1;
   G_Global         : constant System_Global := 19;
   G_Jump           : constant System_Global := 4;
   G_Local          : constant System_Global := 20;
   G_Offset         : constant System_Global := 10;
   G_Remainder      : constant System_Global := 6;
   G_Stack          : constant System_Global := 11;
   G_Trap_Address   : constant System_Global := 13;
   G_Trip_Where     : constant System_Global := 24;
   G_Trip_X         : constant System_Global := 25;
   G_Trip_Y         : constant System_Global := 26;
   G_Trip_Z         : constant System_Global := 27;
   G_Trap_Bootstrap : constant System_Global := 7;
   G_Trap_Where     : constant System_Global := 28;
   G_Trap_X         : constant System_Global := 29;
   G_Trap_Y         : constant System_Global := 30;
   G_Trap_Z         : constant System_Global := 31;

   type Instance
     (Addressable : access Aqua.Addressable.Instance'Class) is tagged limited
      record
         Window     : Window_Register_Array := (others => 0);
         Global     : Global_Register_Array := (others   => 0);
         PC         : Word := 16#C000#;
         Halted     : Boolean := True;
         Privileged : Boolean := True;
         G_B        : Word := 0;
         G_BB       : Word := 0;
         G_D        : Word := 0;
         G_G        : Register_Index := 255;
         G_J        : Word := 0;
         G_L        : Register_Index := 0;
         G_O        : Word := 0;
         G_R        : Word := 0;
         G_S        : Word := 0;
         G_T        : Word := 0;
         G_W        : Word := 0;
         G_X        : Word := 0;
         G_Y        : Word := 0;
         G_Z        : Word := 0;
         G_WW       : Word := 0;
         G_XX       : Word := 0;
         G_YY       : Word := 0;
         G_ZZ       : Word := 0;
      end record;

   function Is_Global
     (This : Instance'Class;
      R    : Register_Index)
      return Boolean
   is (R >= This.G_G);

   function Is_Local
     (This : Instance'Class;
      R    : Register_Index)
      return Boolean
   is (R < This.G_L);

   function Is_Marginal
     (This : Instance'Class;
      R    : Register_Index)
      return Boolean
   is (R >= This.G_L and then R < This.G_G);

   function Get_G (This : Instance'Class;
                   G    : Register_Index)
                   return Word
   is (case G is
          when System_Global  =>
         (case System_Global (G) is
             when G_Bootstrap      => This.G_B,
             when G_Trap_Bootstrap => This.G_BB,
             when G_Dividend       => This.G_D,
             when G_Global         => Word (This.G_G),
             when G_Jump           => This.G_J,
             when G_Local          => Word (This.G_L),
             when G_Offset         => This.G_O,
             when G_Remainder      => This.G_R,
             when G_Stack          => This.G_S,
             when G_Trap_Address   => This.G_T,
             when G_Trip_Where     => This.G_W,
             when G_Trip_X         => This.G_X,
             when G_Trip_Y         => This.G_Y,
             when G_Trip_Z         => This.G_Z,
             when G_Trap_Where     => This.G_WW,
             when G_Trap_X         => This.G_XX,
             when G_Trap_Y         => This.G_YY,
             when G_Trap_Z         => This.G_ZZ,
             when others           => 0),
          when General_Global =>
             This.Global (G));

   procedure Set_G (This : in out Instance'Class;
                    G    : Register_Index;
                    V    : Word);

   function Get_R (This : Instance'Class;
                   R    : Register_Index)
                   return Word;

   procedure Set_R (This : in out Instance'Class;
                    R    : Register_Index;
                    V    : Word);

   procedure Stack_Room (This : in out Instance'Class);
   procedure Stack_Load (This : in out Instance'Class);
   procedure Stack_Store (This : in out Instance'Class);

   procedure Push (This : in out Instance'Class;
                   R    : Register_Index);

   procedure Pop (This : in out Instance'Class;
                  R    : Register_Index);

   procedure Execute
     (This : in out Instance'Class;
      IR   : Word_32);

end Aqua.CPU;
