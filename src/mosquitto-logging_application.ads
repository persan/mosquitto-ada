package Mosquitto.Logging_Application is
   type Application is new Application_Interface with null record;
   overriding procedure On_Connect
     (Self   : not null access Application;
      Mosq   : Handle_Ref;
      Reason : Integer);

   overriding procedure On_Disconnect
     (Self   : not null access Application;
      Mosq   : Handle_Ref;
      Reason : Integer);

   overriding procedure On_Publish
     (Self    : not null access Application;
      Mosq    : Handle_Ref;
      Mid     : Message_Id);

   overriding procedure On_Message
     (Self    : not null access Application;
      Mosq    : Handle_Ref;
      Mid     : Message_Id;
      Topic   : String;
      Payload : Ada.Streams.Stream_Element_Array;
      QoS     : QoS_Type;
      Retain  : Boolean);

   overriding procedure On_Subscribe
     (Self  : not null access Application;
      Mosq  : Handle_Ref;
      Mid   : Message_Id;
      Qoses : Qos_Array);

   overriding procedure On_Unsubscribe
     (Self : not null access Application;
      Mosq : Handle_Ref;
      Mid  : Message_Id);

   overriding procedure On_Log
     (Self    : not null access Application;
      Mosq    : Handle_Ref;
      Level   : Integer;
      Message : String);
   procedure Put_Line
     (Self    : not null access Application;
      Item    : String);

end Mosquitto.Logging_Application;
