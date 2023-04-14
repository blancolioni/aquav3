with WL.String_Maps;

with Aqua.Commands.Parse;

with Aqua.Devices.RAM;
with Aqua.Devices.TTY;

package body Aqua.Devices.Loader is

   package Loader_Maps is
     new WL.String_Maps (Loader_Function);

   Loaders : Loader_Maps.Map;

   ----------
   -- Load --
   ----------

   function Load
     (Command : String)
      return Reference
   is
      Command_Line : constant Commands.Command_Line :=
                       Commands.Parse (Command);
   begin
      if not Loaders.Contains (Command_Line.Command) then
         raise Constraint_Error with
           "no such device: " & Command_Line.Command;
      end if;
      return Loaders.Element (Command_Line.Command) (Command_Line);
   end Load;

   --------------
   -- Register --
   --------------

   procedure Register (Name : String; Loader : Loader_Function) is
   begin
      Loaders.Insert (Name, Loader);
   end Register;

begin
   Register ("ram", Aqua.Devices.RAM.Load'Access);
   Register ("tty", Aqua.Devices.TTY.Load'Access);
end Aqua.Devices.Loader;
