with GNAT.Time_Stamp;
with GNAT.Sockets;
with Ada.Directories;
with Ada.Command_Line;
with Mosquitto.Logging_Application;
procedure Mosquitto.Tests.Main is

   task type Pump (Connection : access Handle) is
      entry Start;
   end Pump;

   task body Pump is
   begin
      accept Start;
      Connection.Loop_Forever;
   end Pump;

   C : aliased Handle;
   A : aliased Logging_Application.Application;
   P : Pump (C'Access);
   function Getpid return Integer;
   pragma Import (C, GetPid, "getpid");
begin
   C.Initialize (GNAT.Sockets.Host_Name & "/" & Ada.Directories.Simple_Name (Ada.Command_Line.Command_Name) & Getpid'Img);
   P.Start;
   C.Set_Handler (A'Unchecked_Access);
   C.Connect ("mqtt");
   C.Subscribe (Topic => "#");
   C.Publish (Mid => null, Topic => "test", Payload => "[" & GNAT.Time_Stamp.Current_Time & "] Hej", Qos => QOS_0, Retain => False);
   delay 1.0;
   C.Disconnect;

end Mosquitto.Tests.Main;
