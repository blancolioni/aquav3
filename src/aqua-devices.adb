package body Aqua.Devices is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out Instance'Class; Command : Aqua.Commands.Command_Line)
   is
   begin
      This.Initialize (Base => Command.Argument ("base"),
                       Bound => Command.Argument ("bound"));
   end Initialize;

end Aqua.Devices;
