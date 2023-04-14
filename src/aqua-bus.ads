private with Ada.Containers.Ordered_Maps;

with Aqua.Addressable;
with Aqua.Devices;

package Aqua.Bus is

   Bus_Error : exception;

   subtype Parent is Aqua.Addressable.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   procedure Install
     (This   : in out Instance'Class;
      Device : not null access Aqua.Devices.Instance'Class);

   procedure Tick (This : in out Instance'Class);

   function Create_Bus return Reference;

private

   package Device_Maps is
     new Ada.Containers.Ordered_Maps (Address_Type, Aqua.Devices.Reference,
                                      "<", Aqua.Devices."=");

   type Instance is new Parent with
      record
         Device_Map : Device_Maps.Map;
      end record;

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8);

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8);

end Aqua.Bus;
