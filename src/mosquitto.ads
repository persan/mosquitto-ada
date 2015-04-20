pragma Ada_2012;
with Ada.Finalization;
with Interfaces.C;
with System;
with Ada.Streams;
package Mosquitto is

   DEFAULT_PORT : constant := 1883;

   type QoS_Type is
     (QOS_0, --  once, with no confirmation
      QoS_1, --  at least once, with confirmation required.
      QoS_2  --  exactly once by using a four step handshake.
     );
   --  Defines how the broker/client will deliver the message

   type Handle is tagged limited private;
   type Handle_Ref is access all Handle'Class;
   pragma No_Strict_Aliasing (Handle_Ref);

   type Message_Id is mod 2 ** 64;

   type Application_Interface is limited interface;
   type Application_Interface_Ref is not null access all Application_Interface'Class;

   function Is_Initialzed (Mosq : Handle) return Boolean;
   function Is_Connected (Mosq : Handle) return Boolean;

   procedure Initialize (Mosq           : in out Handle;
                         ID             : String;
                         Clean_Sessions : Boolean := True); --  with
     --  Pre => ((Id = "" and then Clean_Sessions) or else Id /= "");

   --  Id              String To use As The Client Id. if "", A Random Client Id
   --                  will be generated.
   --  CLean_Session - Set To True To Instruct The Broker To Clean all Messages
   --                  and subscriptions on disconnect, false to instruct it to
   --                  keep them. See the man page mqtt(7) for more details.
   --                  Note that a client will never discard its own outgoing
   --                  messages on disconnect. Calling <connect> or
   --                  <reconnect> will cause the messages to be resent.
   --                  Use <reinitialise> to reset a client to its original state.

   procedure Reinitialise (Mosq           : in out Handle;
                           ID             : String  := "";
                           Clean_Sessions : Boolean := True); -- with
     --  Pre => ((Id = "" and then Clean_Sessions) or else Id /= "");

   procedure Username_Pw_Set (Mosq     : in out Handle;
                              Username : String;
                              Password : String := "") with
     Pre => Mosq.Is_Initialzed;

   procedure Connect (Mosq      : in out Handle;
                      Host      : String := "localhost";
                      Port      : Positive := DEFAULT_PORT;
                      Keepalive : Duration := 0.0) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected,
     Post => Mosq.Is_Connected;

   procedure Set_Handler (Mosq      : in out Handle;
                          Handler   : Application_Interface_Ref) with
     Pre => Mosq.Is_Initialzed;

   procedure Connect_Bind (Mosq         : in out Handle;
                           Host         : String := "localhost";
                           Port         : Positive := DEFAULT_PORT;
                           Keepalive    : Duration := 0.0;
                           Bind_Address : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected,
     Post => Mosq.Is_Connected;

   procedure Connect_Async (Mosq      : in out Handle;
                            Host      : String := "localhost";
                            Port      : Positive := DEFAULT_PORT;
                            Keepalive : Duration := 0.0) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected;

   procedure Connect_Bind_Async (Mosq         : in out Handle;
                                 Host         : String := "localhost";
                                 Port         : Positive := DEFAULT_PORT;
                                 Keepalive    : Duration := 0.0;
                                 Bind_Address : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected;

   procedure Connect_Srv (Mosq         : in out Handle;
                          Host         : String := "localhost";
                          Keepalive    : Duration := 0.0;
                          Bind_Address : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected,
     Post => Mosq.Is_Connected;

   procedure Reconnect (Mosq : in out Handle) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected;

   procedure Reconnect_Async (Mosq : in out Handle) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected;

   procedure Disconnect (Mosq : in out Handle) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected;
   procedure Wait_Until_Connected (Mosq : in out Handle);

   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payloadlen : Natural;
      Payload    : System.Address;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected and then Topic /= "";

   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : String;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected and then Topic /= "";

   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : Ada.Streams.Stream_Element_Array;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected and then Topic /= "";

   generic
      type Msg_Type is private;
   procedure Publish_Generic
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : Msg_Type;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected and then Topic /= "";

   procedure Subscribe
     (Mosq : in out Handle;
      Mid  : access Message_Id := null;
      Topic : String;
      Qos  : QoS_Type := QOS_0) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected and then Topic /= "";

   procedure UnSubscribe
     (Mosq  : in out Handle;
      Mid   : access Message_Id;
      Topic : String) with
     Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected  and then Topic /= "";

   procedure On_Connect
     (self   : not null access Application_Interface;
      Mosq   : Handle_Ref;
      Reason : Integer) is null;

   procedure On_Disconnect
     (self   : not null access Application_Interface;
      Mosq   : Handle_Ref;
      Reason : Integer) is null;

   procedure On_Publish
     (self    : not null access Application_Interface;
      Mosq    : Handle_Ref;
      Mid     : Message_Id) is null;

   procedure On_Message
     (self    : not null access Application_Interface;
      Mosq    : Handle_Ref;
      Mid     : Message_Id;
      Topic   : String;
      Payload : Ada.Streams.Stream_Element_Array;
      QoS     : QoS_Type;
      Retain  : Boolean) is abstract;

   type Qos_Array is array (Natural range <>) of QoS_Type;
   procedure On_Subscribe
     (self  : not null access Application_Interface;
      Mosq  : Handle_Ref;
      Mid   : Message_Id;
      Qoses : Qos_Array) is null;

   procedure On_Unsubscribe
     (self : not null access Application_Interface;
      Mosq : Handle_Ref;
      Mid  : Message_Id) is null;

   procedure On_Log
     (self    : not null access Application_Interface;
      Mosq    : Handle_Ref;
      Level   : Integer;
      Message : String) is null;

   procedure Do_Loop
     (Mosq : in out Handle;
      timeout : Duration := 0.0;
      max_packets : Natural := 1) with
     Pre => Mosq.Is_Initialzed;

   procedure Loop_Forever
     (Mosq : in out Handle;
      timeout : Duration := 0.0;
      max_packets : Natural := 1) with
     Pre => Mosq.Is_Initialzed;

   Mosquitto_Error : exception;

private
   type Null_Application_Type is new Application_Interface with null record;
   overriding procedure On_Message
     (self    : not null access Null_Application_Type;
      Mosq    : Handle_Ref;
      Mid     : Message_Id;
      Topic   : String;
      Payload : Ada.Streams.Stream_Element_Array;
      QoS     : QoS_Type;
      Retain  : Boolean) is null;
   Null_Application : aliased Null_Application_Type;

   protected type Guard_Type is
      procedure Set (Open : Boolean);
      function Get return Boolean;
      entry Wait;
   private
      Is_Open : Boolean := False;
   end Guard_Type;

   use type System.Address;
   type Handle is new Ada.Finalization.Limited_Controlled with record
      Handle    : System.Address := System.Null_Address;
      Applic    : Application_Interface_Ref := Null_Application'Access;
      Connected : Guard_Type;
      Log_Level : Integer := Integer'Last;
   end record;
   overriding procedure Finalize   (Mosq : in out Handle);

   type Controler is new Ada.Finalization.Controlled with null record;
   overriding procedure Initialize (Object : in out Controler);
   overriding procedure Finalize   (Object : in out Controler);
   --
   --  Is used to initialize and finalize the unrerlying library.

   procedure Retcode_2_Exception (Code : Interfaces.C.Int);
   pragma Linker_Options  ("-l" & "mosquitto");

end Mosquitto;
