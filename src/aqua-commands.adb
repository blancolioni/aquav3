with Ada.Strings.Fixed;

package body Aqua.Commands is

   --------------
   -- Argument --
   --------------

   function Argument (This : Command_Line'Class; Name : String) return String
   is
   begin
      for Argument of This.Arguments loop
         declare
            Equal : constant Natural :=
                      Ada.Strings.Fixed.Index (Argument, "=");
         begin
            if Equal > 0
              and then Argument (Argument'First .. Equal - 1) = Name
            then
               return Argument (Equal + 1 .. Argument'Last);
            end if;
         end;
      end loop;
      return "";
   end Argument;

end Aqua.Commands;
