with GNAT.Sockets;
with Ada.Directories;
with Ada.Command_Line;
with GNAT.IO;
with GNAT.Command_Line;
with GNAT.Strings;

procedure Mosquitto.Logging_Application.Main is
   task type Pump (Connection : access Handle) is
      entry Start;
   end Pump;

   task body Pump is
   begin
      accept Start;
      Connection.Loop_Forever;
      delay 0.1;
   end Pump;
   Dummy_Char : Character;
   C : aliased Handle;
   A : aliased Logging_Application.Application;
   P : Pump (C'Access);
   function Getpid return Integer;
   pragma Import (C, GetPid, "getpid");

   Host : aliased GNAT.Strings.String_Access := new String'("mqtt");
   Port : aliased Integer := DEFAULT_PORT;
   ID   : aliased GNAT.Strings.String_Access := new String'(GNAT.Sockets.Host_Name & "/" & Ada.Directories.Simple_Name (Ada.Command_Line.Command_Name) & Getpid'Img);
   Topic  : aliased GNAT.Strings.String_Access := new String'("#");

   Command_Line_Configuration : GNAT.Command_Line.Command_Line_Configuration;
   use GNAT.Command_Line;
begin
   Define_Switch (Command_Line_Configuration, Host'Access, "-h=", "--host=", "Name of mqttserver", Argument => "host");
   Define_Switch (Command_Line_Configuration, Port'Access, "-p=", "--port=", "Port on  mqttserver", Initial => DEFAULT_PORT, Default => DEFAULT_PORT,
                  Argument => "port");
   Define_Switch (Command_Line_Configuration, Id'Access, "-i=", "--id=", "Id odf client", Argument => "id");
   Define_Switch (Command_Line_Configuration, Topic'Access, "-t=", "--topic=", "Topic(s) to subscribe to", Argument => "topic");
   Getopt (Command_Line_Configuration);

   C.Initialize (Id.all);
   P.Start;
   C.Set_Handler (A'Unchecked_Access);
   C.Connect (Host.all);
   C.Subscribe (Topic => Topic.all);
   GNAT.IO.Get (Dummy_Char);
   C.Disconnect;
end Mosquitto.Logging_Application.Main;
