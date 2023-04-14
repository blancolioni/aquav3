package body Aqua.Devices.RAM is

   type Word_8_Array is array (Address_Type range <>) of Word_8;
   type Word_8_Array_Access is access Word_8_Array;

   subtype Parent is Aqua.Devices.Instance;

   type RAM_Instance is new Parent with
      record
         M : Word_8_Array_Access;
      end record;

   overriding function Name (This : RAM_Instance) return String is ("RAM");

   overriding procedure Get_Word_8
     (This : in out RAM_Instance; Address : Address_Type; Value : out Word_8);

   overriding procedure Set_Word_8
     (This : in out RAM_Instance; Address : Address_Type; Value : Word_8);

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This : in out RAM_Instance; Address : Address_Type; Value : out Word_8)
   is
   begin
      Value := This.M (Address);
   end Get_Word_8;

   ----------
   -- Load --
   ----------

   function Load
     (Command : Aqua.Commands.Command_Line)
      return Reference
   is
      This : RAM_Instance;
   begin
      This.Initialize (Command);
      This.M := new Word_8_Array (0 .. This.Bound - This.Base - 1);
      return new RAM_Instance'(This);
   end Load;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This : in out RAM_Instance; Address : Address_Type; Value : Word_8)
   is
   begin
      This.M (Address) := Value;
   end Set_Word_8;

end Aqua.Devices.RAM;
