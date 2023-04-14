with Aqua.Commands;

package Aqua.Devices.Loader is

   type Loader_Function is access
     function (Command : Aqua.Commands.Command_Line)
               return Reference;

   procedure Register
     (Name   : String;
      Loader : Loader_Function);

   function Load
     (Command : String)
      return Reference;

end Aqua.Devices.Loader;
