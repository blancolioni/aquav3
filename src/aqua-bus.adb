with Aqua.Images;

package body Aqua.Bus is

   ----------------
   -- Create_Bus --
   ----------------

   function Create_Bus return Reference is
   begin
      return This : constant Reference := new Instance do
         This.Initialize (16#0000_0000#, 16#FFFF_FFFC#);
      end return;
   end Create_Bus;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This : in out Instance; Address : Address_Type; Value : out Word_8)
   is
      Position : constant Device_Maps.Cursor :=
                   This.Device_Map.Floor (Address);
   begin
      if not Device_Maps.Has_Element (Position) then
         raise Bus_Error with Aqua.Images.Hex_Image (Address);
      else
         declare
            Device : constant Aqua.Devices.Reference :=
                       Device_Maps.Element (Position);
         begin
            if Address not in Device.Base .. Device.Bound - 1 then
               raise Bus_Error with Aqua.Images.Hex_Image (Address);
            end if;
            Device.Get_Word_8 (Address - Device.Base, Value);
         end;
      end if;
   end Get_Word_8;

   -------------
   -- Install --
   -------------

   procedure Install
     (This   : in out Instance'Class;
      Device : not null access Aqua.Devices.Instance'Class)
   is
   begin
      This.Device_Map.Insert (Device.Base, Aqua.Devices.Reference (Device));
   end Install;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This : in out Instance; Address : Address_Type; Value : Word_8)
   is
      Position : constant Device_Maps.Cursor :=
                   This.Device_Map.Floor (Address);
   begin
      if not Device_Maps.Has_Element (Position) then
         raise Bus_Error with Aqua.Images.Hex_Image (Address);
      else
         declare
            Device : constant Aqua.Devices.Reference :=
                       Device_Maps.Element (Position);
         begin
            if Address not in Device.Base .. Device.Bound - 1 then
               raise Bus_Error with Aqua.Images.Hex_Image (Address);
            end if;
            Device.Set_Word_8 (Address - Device.Base, Value);
         end;
      end if;
   end Set_Word_8;

   ----------
   -- Tick --
   ----------

   procedure Tick (This : in out Instance'Class) is
   begin
      for Device of This.Device_Map loop
         Device.Tick;
      end loop;
   end Tick;

end Aqua.Bus;
