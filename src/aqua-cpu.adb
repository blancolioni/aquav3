with Aqua.Images;
with Aqua.Logging;
with Aqua.Options;

with Aqua.Instruction;

package body Aqua.CPU is

   use Aqua.Instruction;

   Enable_Trace : Boolean := False;

   type Instruction_Record is
      record
         Op, X, Y, Z : Word_8;
      end record;

   function Split (IR : Word_32) return Instruction_Record;

   procedure Execute_Trap
     (This    : in out Instance'Class;
      X, Y, Z : Word_8);

   function Execute_ALU
     (This     : in out Instance'Class;
      Y, Z     : Word_32;
      Op       : Instruction.ALU_Operation;
      Unsigned : Boolean)
      return Word_32;

   procedure Execute_Branch
     (This      : in out Instance'Class;
      Value     : Word_32;
      Condition : Condition_Type;
      Negated   : Boolean;
      Backwards : Boolean;
      Offset    : Word_32);

   procedure Execute_Load
     (This        : in out Instance'Class;
      Address     : Address_Type;
      Size        : Op_Size;
      Unsigned    : Boolean;
      Destination : out Word_32);

   procedure Execute_Store
     (This     : in out Instance'Class;
      Address  : Address_Type;
      Size     : Op_Size;
      Unsigned : Boolean;
      Source   : Word_32);

   function To_Window_Index
     (This : Instance'Class;
      R    : Register_Index)
      return Window_Register_Index
   is (Window_Register_Index
       ((This.G_O / 4 + Word (R)) mod Register_Window_Size));

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This : in out Instance'Class;
      IR   : Word_32)
   is
      Rec     : constant Instruction_Record := Split (IR);
      Control : constant Aqua.Instruction.Control_Record :=
                  Aqua.Instruction.Get_Control (Rec.Op);
   begin
      case Control.Class is
         when Undefined =>
            raise Program_Error with
              "bad instruction: " & Aqua.Images.Hex_Image (IR);
         when Trap =>
            This.Execute_Trap (Rec.X, Rec.Y, Rec.Z);
         when ALU =>
            declare
               ZZ : constant Word_32 :=
                      (if Control.Immediate_Z
                       then Word_32 (Rec.Z)
                       else This.Get_R (Register_Index (Rec.Z)));
               YY : constant Word_32 :=
                      This.Get_R (Register_Index (Rec.Y));
               XX : constant Word_32 :=
                      This.Execute_ALU (YY, ZZ,
                                        Control.ALU_Op, Control.Unsigned);
               X_Name : String := Rec.X'Image;
            begin
               X_Name (X_Name'First) := '%';
               Aqua.Logging.Log
                 (X_Name & " <- " & Aqua.Images.Hex_Image (XX));
               This.Set_R (Register_Index (Rec.X), XX);
            end;
         when Load_Store =>
            if Control.Relative_Addr then
               declare
                  YZ      : constant Word_32 :=
                              Word_32 (Rec.Y) * 256 + Word_32 (Rec.Z);
                  Address : constant Word_32 :=
                              This.PC - 4
                                + (4 * YZ
                                   - (if Control.Backwards
                                     then 262144 else 0));
               begin
                  This.Set_R (Register_Index (Rec.X), Address);
               end;
            else
               declare
                  ZZ      : constant Word_32 :=
                              (if Control.Immediate_Z
                               then Word_32 (Rec.Z)
                               else This.Get_R (Register_Index (Rec.Z)));
                  YY      : constant Word_32 :=
                              This.Get_R (Register_Index (Rec.Y));
                  XX      : Word_32;
                  Address : constant Address_Type := Address_Type (YY + ZZ);
               begin
                  if Control.Load then
                     This.Execute_Load
                       (Address, Control.Size, Control.Unsigned, XX);
                     This.Set_R (Register_Index (Rec.X), XX);
                  else
                     XX := This.Get_R (Register_Index (Rec.X));
                     This.Execute_Store
                       (Address, Control.Size, Control.Unsigned, XX);
                  end if;
               end;
            end if;
         when Conditional_Set =>
            declare
               YY : constant Word_32 :=
                      This.Get_R (Register_Index (Rec.Y));
               ZZ : constant Word_32 :=
                      (if Control.Immediate_Z
                       then Word_32 (Rec.Z)
                       else This.Get_R (Register_Index (Rec.Z)));
               Sat : constant Boolean :=
                       Control.Negated xor
                           (case Control.Condition is
                               when Cond_Negative =>
                                 YY >= 16#1000_0000#,
                               when Cond_Zero     =>
                                 YY = 0,
                               when Cond_Positive =>
                                 YY < 16#1000_0000#,
                               when Cond_Odd      =>
                                 YY mod 2 = 1);
            begin
               if Sat then
                  This.Set_R (Register_Index (Rec.X), ZZ);
               elsif Control.Zero_Or_Set then
                  This.Set_R (Register_Index (Rec.X), 0);
               end if;
            end;

         when Flow =>
            if Control.Conditional then
               declare
                  XX : constant Word_32 :=
                         This.Get_R (Register_Index (Rec.X));
                  YZ : constant Word_32 :=
                         Word_32 (Rec.Y) * 256 + Word_32 (Rec.Z);
               begin
                  This.Execute_Branch
                    (Value     => XX,
                     Condition => Control.Condition,
                     Negated   => Control.Negated,
                     Backwards => Control.Backwards,
                     Offset    => YZ);
               end;
            elsif Control.Relative_Addr then
               if Control.Push then
                  declare
                     YZ : constant Word_32 :=
                            Word_32 (Rec.Y) * 256 + Word_32 (Rec.Z);
                     Address : constant Word_32 :=
                                 This.PC - 4
                                   + (4 * YZ
                                      - (if Control.Backwards
                                        then 2 ** 18 else 0));
                  begin
                     This.G_J := This.PC;
                     Aqua.Logging.Log ("push: rJ = "
                                       & Aqua.Images.Hex_Image (This.G_J));
                     This.Push (Register_Index (Rec.X));
                     This.PC := Address;
                  end;
               else
                  declare
                     XYZ     : constant Word_32 :=
                                 Word_32 (Rec.X) * 65536
                                 + Word_32 (Rec.Y) * 256
                                    + Word_32 (Rec.Z);
                     Address : constant Word_32 :=
                                 This.PC - 4
                                   + (4 * XYZ
                                      - (if Control.Backwards
                                        then 2 ** 26 else 0));
                  begin
                     This.PC := Address;
                  end;
               end if;
            end if;
         when YZ_Immediate =>
            declare
               High  : constant Boolean := Control.YZ_High;
               Shift : constant Natural := (if High then 16 else 0);
               Mask  : constant Word_32 :=
                         (if High then 16#0000_FFFF# else 16#FFFF_0000#);

               YZ : constant Word_16 :=
                      Word_16 (Rec.Z) + 256 * Word_16 (Rec.Y);
               XX : Word_32 :=
                      This.Get_R (Register_Index (Rec.X));
               W  : Word_16 :=
                         Word_16 (XX / 2 ** Shift mod 2 ** 16);
            begin
               case Control.YZ_Op is
                  when Op_Set =>
                     W := YZ;
                  when Op_Inc =>
                     XX := XX + Word_32 (YZ) * 2 ** Shift;
                  when Op_And =>
                     W := W and YZ;
                  when Op_Or =>
                     W := W or YZ;
               end case;

               if Control.YZ_Op = Op_Set then
                  This.Set_R (Register_Index (Rec.X),
                              Word_32 (W) * 2 ** Shift);
               elsif Control.YZ_Op = Op_Inc then
                  This.Set_R (Register_Index (Rec.X), XX);
               else
                  This.Set_R (Register_Index (Rec.X),
                              (XX and Mask) or Word_32 (W) * 2 ** Shift);
               end if;
            end;
         when Misc =>
            case Control.Misc_Op is
               when Get =>
                  This.Set_R (Register_Index (Rec.X),
                              This.Get_G (Register_Index (Rec.Z mod 32)));
               when Pop =>

                  Aqua.Logging.Log ("pop: rJ = "
                                    & Aqua.Images.Hex_Image (This.G_J));

                  This.Pop (Register_Index (Rec.X));
                  This.PC := This.G_J;
               when Put =>
                  declare
                     ZZ : constant Word_32 :=
                            (if Control.Immediate_Z
                             then 256 * Word_32 (Rec.Y) + Word_32 (Rec.Z)
                             else This.Get_R (Register_Index (Rec.Z)));
                  begin
                     This.Set_G (Register_Index (Rec.X mod 32), ZZ);
                  end;

               when Resume =>
                  declare
                     Trap : constant Instruction_Record :=
                              Split (This.Get_G (G_Trap_X));
                  begin
                     This.PC := This.Get_G (G_Trap_Where);
                     This.Set_R (Register_Index (Trap.Y),
                                 This.Get_G (G_Trap_Y));
                     This.Set_R (Register_Index (Trap.Z),
                                 This.Get_G (G_Trap_Z));
                  end;
            end case;
      end case;

      This.Addressable.Tick;

   end Execute;

   -----------------
   -- Execute_ALU --
   -----------------

   function Execute_ALU
     (This     : in out Instance'Class;
      Y, Z     : Word_32;
      Op       : Instruction.ALU_Operation;
      Unsigned : Boolean)
      return Word_32
   is
      pragma Unreferenced (This);

      function Sign (X : Word_32) return Word_32
      is (if X = 0 then 0
          elsif X < 16#1000_0000# then 1
          else 16#FFFF_FFFF#);

      function Shift_Left (X : Word_32; Bits : Natural) return Word_32;

      function Shift_Right (X : Word_32; Bits : Natural) return Word_32;

      ----------------
      -- Shift_Left --
      ----------------

      function Shift_Left (X : Word_32; Bits : Natural) return Word_32 is
      begin
         if Bits < 32 then
            return X * 2 ** Bits;
         else
            return 0;
         end if;
      end Shift_Left;

      -----------------
      -- Shift_Right --
      -----------------

      function Shift_Right (X : Word_32; Bits : Natural) return Word_32 is
      begin
         if Bits < 32 then
            if Unsigned
              or else X < 16#1000_0000#
            then
               return X / 2 ** Bits;
            else
               return X / 2 ** Bits or not (2 ** (32 - Bits) - 1);
            end if;
         else
            if Unsigned then
               return 0;
            else
               return 16#FFFF_FFFF#;
            end if;
         end if;
      end Shift_Right;

      X : constant Word_32 :=
            (case Op is
                when Op_Add => Y + Z,
                when Op_Sub => Y - Z,
                when Op_Mul => Y * Z,
                when Op_Div => Y / Z,
                when Op_Neg => (not Z) + 1,
                when Op_And => Y and Z,
                when Op_Or  => Y or Z,
                when Op_Xor => Y xor Z,
                when Op_Cmp => Sign (Y - Z),
                when Op_Sl  => Shift_Left (Y, Natural (Z)),
                when Op_Sr  => Shift_Right (Y, Natural (Z)));
   begin
      if Op = Op_Div then
         This.G_R := Y mod Z;
      end if;
      return X;
   end Execute_ALU;

   --------------------
   -- Execute_Branch --
   --------------------

   procedure Execute_Branch
     (This      : in out Instance'Class;
      Value     : Word_32;
      Condition : Condition_Type;
      Negated   : Boolean;
      Backwards : Boolean;
      Offset    : Word_32)
   is
      Taken : constant Boolean :=
                Negated xor
                    (case Condition is
                        when Cond_Negative =>
                          Value >= 16#1000_0000#,
                        when Cond_Zero     =>
                          Value = 0,
                        when Cond_Positive =>
                          Value < 16#1000_0000#,
                        when Cond_Odd      =>
                          Value mod 2 = 1);
   begin
      if Taken then
         This.PC := This.PC - 4
           + (4 * Offset
              - (if Backwards then 262144 else 0));
      end if;
   end Execute_Branch;

   ------------------
   -- Execute_Load --
   ------------------

   procedure Execute_Load
     (This        : in out Instance'Class;
      Address     : Address_Type;
      Size        : Op_Size;
      Unsigned    : Boolean;
      Destination : out Word_32)
   is
   begin
      case Size is
         when Size_8 =>
            declare
               W : Word_8;
            begin
               This.Addressable.Get_Word_8 (Address, W);
               if Unsigned or else W < 128 then
                  Destination := Word_32 (W);
               else
                  Destination := 16#FFFF_FF00# or Word_32 (W);
               end if;
            end;
         when Size_16 =>
            declare
               W : Word_16;
            begin
               This.Addressable.Get_Word_16 (Address, W);
               if Unsigned or else W < 32768 then
                  Destination := Word_32 (W);
               else
                  Destination := 16#FFFF_0000# or Word_32 (W);
               end if;
            end;
         when Size_32 =>
            declare
               W : Word_32;
            begin
               This.Addressable.Get_Word_32 (Address, W);
               if Enable_Trace then
                  Aqua.Logging.Log
                    (Aqua.Images.Hex_Image (Address)
                     & " -> "
                     & Aqua.Images.Hex_Image (W));
               end if;
               Destination := W;
            end;
      end case;
   end Execute_Load;

   -------------------
   -- Execute_Store --
   -------------------

   procedure Execute_Store
     (This     : in out Instance'Class;
      Address  : Address_Type;
      Size     : Op_Size;
      Unsigned : Boolean;
      Source   : Word_32)
   is
      pragma Unreferenced (Unsigned);
   begin
      case Size is
         when Size_8 =>
            This.Addressable.Set_Word_8 (Address, Word_8 (Source mod 256));
         when Size_16 =>
            This.Addressable.Set_Word_16 (Address, Word_16 (Source mod 65536));
         when Size_32 =>
            This.Addressable.Set_Word_32 (Address, Source);
      end case;
   end Execute_Store;

   ------------------
   -- Execute_Trap --
   ------------------

   procedure Execute_Trap
     (This    : in out Instance'Class;
      X, Y, Z : Word_8)
   is
   begin
      if X = 0 and then Y = 0 and then Z = 0 then
         This.Halted := True;
      else
         This.G_WW := This.PC;
         This.G_XX :=
           2 ** 31 + Word_32 (X) * 65536
           + Word_32 (Y) * 256
           + Word_32 (Z);
         This.G_YY := This.Get_R (Register_Index (Y));
         This.G_ZZ := This.Get_R (Register_Index (Z));
         This.G_BB := This.Get_R (255);
         This.Set_R (255, This.G_J);
         This.PC := This.G_T;
      end if;
   end Execute_Trap;

   -----------
   -- Get_R --
   -----------

   function Get_R
     (This : Instance'Class;
      R    : Register_Index)
      return Word
   is
   begin
      if This.Is_Marginal (R) then
         return 0;
      elsif This.Is_Local (R) then
         return This.Window (This.To_Window_Index (R));
      else
         return This.Get_G (R);
      end if;
   end Get_R;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (This : in out Instance'Class;
      R    : Register_Index)
   is
      G_L : Register_Index renames This.G_L;
      G_G : Register_Index renames This.G_G;
      G_O : Word renames This.G_O;
      G_S : Word renames This.G_S;
      X   : constant Register_Index :=
              (if R <= G_L then R else G_L + 1);
      Y   : Word := 0;

      function RO return Window_Register_Index
      is (Window_Register_Index (G_O / 4 mod Register_Window_Size));

      function RS return Window_Register_Index
      is (Window_Register_Index (G_S / 4 mod Register_Window_Size));

   begin

      if RO = RS then
         This.Stack_Load;
      end if;

      G_O := G_O - 4;

      if X > 0 and then X <= G_L then
         Y := This.Get_R (X);
      end if;

      declare
         Hole : constant Word := This.Get_R (0);
         L    : Word := Word (G_L);
      begin
         while G_O - G_S < Hole * 4 loop
            --  Aqua.Logging.Log ("stack load: o="
            --                    & Aqua.Images.Hex_Image (G_O)
            --                    & ";s="
            --                    &  Aqua.Images.Hex_Image (G_S));
            This.Stack_Load;
         end loop;

         L := Word'Min (L + 1, Word (X)) + Hole;
         L := Word'Min (L, Word (G_G));

         if Hole < L then
            This.Window (RO) := Y;
         end if;

         This.Set_G (G_Local, L);
         This.Set_G (G_Offset, G_O - Hole * 4);

         --  G_L := Register_Index (L);
         --  G_O := G_O - Hole * 4;
      end;

   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance'Class;
                   R    : Register_Index)
   is
      G_L : Register_Index renames This.G_L;
      G_O : Word renames This.G_O;
      X   : Register_Index := R;
   begin
      if This.Is_Global (X) then
         X := G_L;
         G_L := G_L + 1;
         This.Stack_Room;
      end if;

      This.Set_R (X, Word (X));

      This.Set_G (G_Local, Word (G_L - X - 1));
      This.Set_G (G_Offset, G_O + 4 * (Word (X) + 1));

      --  G_L := G_L - X - 1;
      --  G_O := G_O + 4 * (Word (X) + 1);

   end Push;

   -----------
   -- Set_G --
   -----------

   procedure Set_G
     (This : in out Instance'Class;
      G    : Register_Index;
      V    : Word)
   is
   begin
      pragma Assert (G /= G_Global or else V mod 256 >= 32,
                     "G must be >= 32");
      pragma Assert (G /= G_Local or else V mod 256 < Word (This.G_G),
                    "L must be < G");

      case G is
         when System_Global =>
            if Enable_Trace then
               declare
                  Img : String := G'Image;
               begin
                  Img (Img'First) := 'r';
                  Aqua.Logging.Log
                    (Img & " <- " & Aqua.Images.Hex_Image (V));
               end;
            end if;

            case System_Global (G) is
               when G_Bootstrap   =>
                  This.G_B := V;
               when G_Global =>
                  This.G_G := Register_Index (V mod 256);
               when G_Jump =>
                  This.G_J := V;
               when G_Local  =>
                  This.G_L := Register_Index (V mod 256);
               when G_Offset =>
                  This.G_O := V;
               when G_Stack =>
                  This.G_S := V;
               when G_Trap_Address   =>
                  This.G_T := V;
               when G_Trip_Where     =>
                  This.G_W := V;
               when G_Trip_X         =>
                  This.G_X := V;
               when G_Trip_Y         =>
                  This.G_Y := V;
               when G_Trip_Z         =>
                  This.G_Z := V;
               when G_Trap_Bootstrap =>
                  This.G_BB := V;
               when G_Trap_Where     =>
                  This.G_WW := V;
               when G_Trap_X         =>
                  This.G_XX := V;
               when G_Trap_Y         =>
                  This.G_YY := V;
               when G_Trap_Z         =>
                  This.G_ZZ := V;
               when others =>
                  raise Constraint_Error with
                    "cannot set special register" & G'Image;
            end case;
         when General_Global =>
            This.Global (G) := V;
      end case;
   end Set_G;

   -----------
   -- Set_R --
   -----------

   procedure Set_R
     (This : in out Instance'Class;
      R    : Register_Index;
      V    : Word)
   is
   begin
      while This.Is_Marginal (R) loop
         This.Set_G (G_Local, This.Get_G (G_Local) + 1);
         This.Stack_Room;
      end loop;

      if Enable_Trace then
         declare
            Img : String := R'Image;
         begin
            Img (Img'First) := '%';
            Aqua.Logging.Log
              (Img & " <- " & Aqua.Images.Hex_Image (V));
         end;
      end if;

      if This.Is_Local (R) then
         declare
            W : constant Window_Register_Index :=
                  Window_Register_Index
                    (This.G_O / 4 mod Register_Window_Size)
                  + Window_Register_Index (R);
         begin
            This.Window (W) := V;
         end;
      else
         This.Set_G (R, V);
      end if;

   end Set_R;

   -----------
   -- Split --
   -----------

   function Split (IR : Word_32) return Instruction_Record is
      It : Word_32 := IR;

      function Next return Word_8;

      ----------
      -- Next --
      ----------

      function Next return Word_8 is
      begin
         return W : constant Word_8 := Word_8 (It mod 256) do
            It := It / 256;
         end return;
      end Next;

   begin
      return Rec : Instruction_Record do
         Rec.Z := Next;
         Rec.Y := Next;
         Rec.X := Next;
         Rec.Op := Next;
      end return;
   end Split;

   ----------------
   -- Stack_Load --
   ----------------

   procedure Stack_Load (This : in out Instance'Class) is
      G_S : Word renames This.G_S;
   begin
      G_S := G_S - 4;

      This.Addressable.Get_Word_32
        (Address => Address_Type (G_S),
         Value   =>
           This.Window
             (Window_Register_Index (G_S / 4 mod Register_Window_Size)));

   end Stack_Load;

   ----------------
   -- Stack_Room --
   ----------------

   procedure Stack_Room (This : in out Instance'Class) is
      G_L : Register_Index renames This.G_L;
      G_O : Word renames This.G_O;
      G_S : Word renames This.G_S;
   begin
      if (G_S - G_O) / 4 mod Register_Window_Size = Word (G_L) then
         This.Stack_Store;
      end if;
   end Stack_Room;

   -----------------
   -- Stack_Store --
   -----------------

   procedure Stack_Store (This : in out Instance'Class) is
      G_S : Word renames This.G_S;
   begin
      This.Addressable.Set_Word_32
        (Address => Address_Type (G_S),
         Value   =>
           This.Window
             (Window_Register_Index (G_S / 4 mod Register_Window_Size)));
      G_S := G_S + 4;
   end Stack_Store;

   -----------
   -- Start --
   -----------

   procedure Start
     (This             : in out Instance'Class;
      Initial_Location : Address_Type)
   is
   begin
      Enable_Trace := Aqua.Options.Trace;

      This.PC := Initial_Location;
      This.Halted := False;
      while not This.Halted loop
         declare
            IR : Word;
         begin
            This.Addressable.Get_Word_32 (This.PC, IR);
            Aqua.Logging.Log
              (Aqua.Images.Hex_Image (This.PC)
               & ": "
               & Aqua.Images.Hex_Image (IR));
            This.PC := This.PC + 4;
            This.Execute (IR);
         end;
      end loop;
   end Start;

end Aqua.CPU;
