with Ada.Directories;
with Ada.Text_IO;

with Aqua.Config;
with Aqua.Images;
with Aqua.Options;
with Aqua.Paths;

with Aqua.Bus;
with Aqua.Devices.Loader;

package body Aqua.Loader is

   ----------------
   -- Create_CPU --
   ----------------

   function Create_CPU return Aqua.CPU.Reference is
   begin
      if not Ada.Directories.Exists (".aqua-config") then
         Ada.Directories.Copy_File
           (Source_Name => Aqua.Paths.Config_File ("aqua.config"),
            Target_Name => ".aqua-config");
      end if;

      Aqua.Config.Load_Configuration (".aqua-config");

      declare
         Bus : constant Aqua.Bus.Reference := Aqua.Bus.Create_Bus;

         procedure Install_Device (Command : String);

         --------------------
         -- Install_Device --
         --------------------

         procedure Install_Device (Command : String) is
            Device : constant Aqua.Devices.Reference :=
                    Aqua.Devices.Loader.Load (Command);
         begin
            if Aqua.Options.Show_Devices then
               Ada.Text_IO.Put (Device.Name);
               Ada.Text_IO.Set_Col (20);
               Ada.Text_IO.Put (Aqua.Images.Hex_Image (Device.Base));
               Ada.Text_IO.Set_Col (30);
               Ada.Text_IO.Put (Aqua.Images.Hex_Image (Device.Bound - 1));
               Ada.Text_IO.New_Line;
            end if;

            Bus.Install (Device);
         end Install_Device;

      begin
         Aqua.Config.Iterate_Config ("device", Install_Device'Access);

         return new Aqua.CPU.Instance (Bus);
      end;

   end Create_CPU;

end Aqua.Loader;
