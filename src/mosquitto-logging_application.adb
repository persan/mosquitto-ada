with Gnat.IO;
with GNAT.Source_Info;
with GNAT.Time_Stamp;
package body Mosquitto.Logging_Application is
   use Gnat.IO;
   use GNAT.Source_Info;
   use GNAT.Time_Stamp;

   procedure Put_Line
     (Self    : not null access Application;
      Item    : String) is
      pragma Unreferenced (Self);
   begin
      Put_Line (Current_Time & ":" & Item);
   end;

   ----------------
   -- On_Connect --
   ----------------

   overriding procedure On_Connect
     (Self   : not null access Application;
      Mosq   : Handle_Ref;
      Reason : Integer)
   is
      pragma Unreferenced (Mosq);
   begin
      Application'Class (Self.all).Put_Line (Enclosing_Entity & " (Reason =>" & Reason'Img & ")");
   end On_Connect;

   -------------------
   -- On_Disconnect --
   -------------------

   overriding procedure On_Disconnect
     (Self   : not null access Application;
      Mosq   : Handle_Ref;
      Reason : Integer)
   is
      pragma Unreferenced (Mosq);
   begin
      Application'Class (Self.all).Put_Line (Enclosing_Entity & " (Reason=>" & Reason'Img & ")");
   end On_Disconnect;

   ----------------
   -- On_Publish --
   ----------------

   overriding procedure On_Publish
     (Self    : not null access Application;
      Mosq    : Handle_Ref;
      Mid     : Message_Id)
   is
      pragma Unreferenced (Mosq);
   begin
      Application'Class (Self.all).Put_Line (Enclosing_Entity & " (Mid =>" & Mid'Img & ")");
   end On_Publish;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Self    : not null access Application;
      Mosq    : Handle_Ref;
      Mid     : Message_Id;
      Topic   : String;
      Payload : Ada.Streams.Stream_Element_Array;
      QoS     : QoS_Type;
      Retain  : Boolean)
   is
      Overlayed_Data : String (Natural (Payload'First) .. Natural (Payload'Last));
      pragma Import (C, Overlayed_Data);
      for Overlayed_Data'Address use Payload'Address;
      pragma Unreferenced (Mosq);
   begin
      Application'Class (Self.all).Put_Line (Enclosing_Entity &
                       " (Mid =>" & Mid'Img & "," &
                       " Topic => """ & Topic & """," &
                       " Payload => """ & Overlayed_Data & """"  &
                       " QoS => " & QoS'Img & "," &
                       " Retain => " & Retain'Img &
                       ")");
   end On_Message;

   ------------------
   -- On_Subscribe --
   ------------------

   overriding procedure On_Subscribe
     (Self  : not null access Application;
      Mosq  : Handle_Ref;
      Mid   : Message_Id;
      Qoses : Qos_Array)
   is
      pragma Unreferenced (Mosq, Qoses);
   begin
      Application'Class (Self.all).Put_Line (Enclosing_Entity & " (Mid =>" & Mid'Img & ")");
   end On_Subscribe;

   --------------------
   -- On_Unsubscribe --
   --------------------

   overriding procedure On_Unsubscribe
     (Self : not null access Application;
      Mosq : Handle_Ref;
      Mid  : Message_Id)
   is
      pragma Unreferenced (Mosq);
   begin
      Application'Class (Self.all).Put_Line (Enclosing_Entity & " (Mid =>" & Mid'Img & ")");
   end On_Unsubscribe;

   ------------
   -- On_Log --
   ------------

   overriding procedure On_Log
     (Self    : not null access Application;
      Mosq    : Handle_Ref;
      Level   : Integer;
      Message : String)
   is
      pragma Unreferenced (Mosq);
   begin
      Application'Class (Self.all).Put_Line (Enclosing_Entity & " (Level =>" & Level'Img & ", Message => """ & Message & """)");
   end On_Log;

end Mosquitto.Logging_Application;
