pragma Ada_2012;
with Ada.Finalization;
with Interfaces.C;
with System;
with Ada.Streams;
package Mosquitto is

   DEFAULT_PORT       : constant := 1883;
   Max_Message_Length : constant := 16#0FFF_FFFF#;
   ID_MAX_LENGTH      : constant := 23;

   subtype Message_Length is Natural range 0 .. Max_Message_Length;

   type QoS_Type is
     (QOS_0, --  once, with no confirmation
      QoS_1, --  at least once, with confirmation required.
      QoS_2  --  exactly once by using a four step handshake.
     );
   --  Defines how the broker/client will deliver the message

   type Handle is tagged limited private;
   type Handle_Ref is access all Handle'Class with Storage_Size => 0;
   pragma No_Strict_Aliasing (Handle_Ref);

   type Message_Id is mod 2 ** 64;

   type Application_Interface is limited interface;
   type Application_Interface_Ref is not null access all Application_Interface'Class;

   not overriding function Is_Initialzed (Mosq : Handle) return Boolean;
   not overriding function Is_Connected (Mosq : Handle) return Boolean;

   procedure Initialize (Mosq           : in out Handle;
                         ID             : String;
                         Clean_Sessions : Boolean := True)  with
     Pre => ((Id = "" and then Clean_Sessions) or else Id /= "") and then not Mosq.Is_Initialzed and then Id'Length <= ID_MAX_LENGTH;

   --  Id              String To use As The Client Id. if "", A Random Client Id
   --                  will be generated.
   --  CLean_Session - Set To True To Instruct The Broker To Clean all Messages
   --                  and subscriptions on disconnect, false to instruct it to
   --                  keep them. See the man page mqtt(7) for more details.
   --                  Note that a client will never discard its own outgoing
   --                  messages on disconnect. Calling <connect> or
   --                  <reconnect> will cause the messages to be resent.
   --                  Use <reinitialise> to reset a client to its original state.
   not overriding
   procedure Reinitialise (Mosq           : in out Handle;
                           ID             : String  := "";
                           Clean_Sessions : Boolean := True) with
     Pre => ((Id = "" and then Clean_Sessions) or else Id /= "") and then Mosq.Is_Initialzed and then Id'Length <= ID_MAX_LENGTH;
   --  Id              String To use As The Client Id. if "", A Random Client Id
   --                  will be generated.
   --  CLean_Session - Set To True To Instruct The Broker To Clean all Messages
   --                  and subscriptions on disconnect, false to instruct it to
   --                  keep them. See the man page mqtt(7) for more details.
   --                  Note that a client will never discard its own outgoing
   --                  messages on disconnect. Calling <connect> or
   --                  <reconnect> will cause the messages to be resent.
   --                  Use <reinitialise> to reset a client to its original state.

   not overriding
   procedure Username_Pw_Set (Mosq     : in out Handle;
                              Username : String;
                              Password : String := "") with
     Pre => Mosq.Is_Initialzed;
   --  username - the username to send as a string, or "" to disable
   --             authentication.
   --  password - the password to send as a string. Set to "" when username is
   --             valid in order to send just a username.

   not overriding
   procedure Will_Set
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payloadlen : Message_Length;
      Payload    : System.Address;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => not Mosq.Is_Connected and then Topic /= "";
   not overriding

   procedure Will_Set
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : aliased String;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => not Mosq.Is_Connected and then Topic /= "" and then Payload'Length <= Max_Message_Length;

   not overriding
   procedure Will_Set
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : aliased Ada.Streams.Stream_Element_Array;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => not Mosq.Is_Connected and then Topic /= "" and then Payload'Length <= Max_Message_Length;

   generic
      type Msg_Type is private;
      pragma Compile_Time_Error
        (Msg_Type'Size / Ada.Streams.Stream_Element'Size <= Max_Message_Length,
         "Size of Msg_Type is to large");
   procedure Will_Set_Generic
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : aliased Msg_Type;
      Qos        : QoS_Type;
      Retain     : Boolean); --  with
                             --  Pre => not Mosq.Is_Connected and then Topic /= "" ;

   not overriding
   procedure Will_Clear
     (Mosq       : in out Handle) with
     Pre => not Mosq.Is_Connected;

   not overriding
   procedure Connect (Mosq      : in out Handle;
                      Host      : String := "localhost";
                      Port      : Positive := DEFAULT_PORT;
                      Keepalive : Duration := 0.0) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected,
     Post => Mosq.Is_Connected;
   --  host -      the hostname or ip address of the broker to connect to.
   --  port -      the network port to connect to.
   --  keepalive - the number of seconds after which the broker should send a PING
   --             message to the client if no other messages have been exchanged
   --             in that time.

   not overriding
   procedure Set_Handler (Mosq      : in out Handle;
                          Handler   : Application_Interface_Ref) with
     Pre => Mosq.Is_Initialzed;

   not overriding
   procedure Connect_Bind (Mosq         : in out Handle;
                           Host         : String := "localhost";
                           Port         : Positive := DEFAULT_PORT;
                           Keepalive    : Duration := 0.0;
                           Bind_Address : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected,
     Post => Mosq.Is_Connected;

   not overriding
   procedure Connect_Async (Mosq      : in out Handle;
                            Host      : String := "localhost";
                            Port      : Positive := DEFAULT_PORT;
                            Keepalive : Duration := 0.0) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected;

   not overriding
   procedure Connect_Bind_Async (Mosq         : in out Handle;
                                 Host         : String := "localhost";
                                 Port         : Positive := DEFAULT_PORT;
                                 Keepalive    : Duration := 0.0;
                                 Bind_Address : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected;

   not overriding
   procedure Connect_Srv (Mosq         : in out Handle;
                          Host         : String := "localhost";
                          Keepalive    : Duration := 0.0;
                          Bind_Address : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected,
     Post => Mosq.Is_Connected;

   not overriding
   procedure Reconnect (Mosq : in out Handle) with
     Pre => Mosq.Is_Connected;

   not overriding
   procedure Reconnect_Async (Mosq : in out Handle) with
     Pre => Mosq.Is_Connected;

   not overriding
   procedure Disconnect (Mosq : in out Handle) with
     Pre => Mosq.Is_Connected;

   not overriding
   procedure Wait_Until_Connected (Mosq : in out Handle) with
     Post => Mosq.Is_Connected;

   not overriding
   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payloadlen : Message_Length;
      Payload    : System.Address;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => Mosq.Is_Connected and then Topic /= "";

   not overriding
   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : String;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => Mosq.Is_Connected and then Topic /= "" and then Payload'Length <= Max_Message_Length;

   not overriding
   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : Ada.Streams.Stream_Element_Array;
      Qos        : QoS_Type;
      Retain     : Boolean) with
     Pre => Mosq.Is_Connected and then Topic /= "" and then Payload'Length <= Max_Message_Length;

   generic
      type Msg_Type is private;
      pragma Compile_Time_Error
        (Msg_Type'Size / Ada.Streams.Stream_Element'Size <= Max_Message_Length,
         "Size of Msg_Type is to large");
   procedure Publish_Generic
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : Msg_Type;
      Qos        : QoS_Type;
      Retain     : Boolean); --  with
   --  Pre => Mosq.Is_Initialzed and then Mosq.Is_Connected and then Topic /= "" ;

   not overriding
   procedure Subscribe
     (Mosq  : in out Handle;
      Mid   : access Message_Id := null;
      Topic : String;
      Qos   : QoS_Type := QOS_0) with
     Pre => Mosq.Is_Connected and then Topic /= "";

   not overriding
   procedure UnSubscribe
     (Mosq  : in out Handle;
      Mid   : access Message_Id;
      Topic : String) with
     Pre => Mosq.Is_Connected  and then Topic /= "";

   not overriding
   procedure On_Connect
     (Self   : not null access Application_Interface;
      Mosq   : Handle_Ref;
      Reason : Integer) is null;

   not overriding
   procedure On_Disconnect
     (Self   : not null access Application_Interface;
      Mosq   : Handle_Ref;
      Reason : Integer) is null;

   not overriding
   procedure On_Publish
     (Self    : not null access Application_Interface;
      Mosq    : Handle_Ref;
      Mid     : Message_Id) is null;

   not overriding
   procedure On_Message
     (Self    : not null access Application_Interface;
      Mosq    : Handle_Ref;
      Mid     : Message_Id;
      Topic   : String;
      Payload : Ada.Streams.Stream_Element_Array;
      QoS     : QoS_Type;
      Retain  : Boolean) is abstract;

   type Qos_Array is array (Natural range <>) of QoS_Type;
   not overriding
   procedure On_Subscribe
     (Self  : not null access Application_Interface;
      Mosq  : Handle_Ref;
      Mid   : Message_Id;
      Qoses : Qos_Array) is null;

   not overriding
   procedure On_Unsubscribe
     (Self : not null access Application_Interface;
      Mosq : Handle_Ref;
      Mid  : Message_Id) is null;

   not overriding
   procedure On_Log
     (Self    : not null access Application_Interface;
      Mosq    : Handle_Ref;
      Level   : Integer;
      Message : String) is null;

   type Pw_Callback_Function is access function (Mosq : in Handle'Class) return String;

   procedure Tls_Set
     (Mosq        : in out Handle;
      Cafile      : String;
      Capath      : String;
      Certfile    : String;
      Keyfile     : String;
      Pw_Callback : Pw_Callback_Function) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected;

   procedure Tls_Insecure_Set (Mosq : Handle; Value : Boolean) with
     Pre => Mosq.Is_Initialzed;

   type Verification_Mode is (SSL_VERIFY_NONE, SSL_VERIFY_PEER);

   procedure Tls_Opts_Set
     (Mosq        : Handle;
      Cert_Reqs   : Verification_Mode;
      Tls_Version : String;
      Ciphers     : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected;

   procedure Tls_Psk_Set
     (Mosq     : Handle;
      Psk      : String;
      Identity : String;
      Ciphers  : String) with
     Pre => Mosq.Is_Initialzed and then not Mosq.Is_Connected;

   DEFAULT_TIME_OUT : constant Duration := 1.0;

   not overriding
   procedure Do_Loop
     (Mosq        : in out Handle;
      Timeout     : Duration := DEFAULT_TIME_OUT;
      Max_Packets : Natural := 1) with
     Pre => Mosq.Is_Initialzed;

   not overriding
   procedure Loop_Forever
     (Mosq        : in out Handle;
      Timeout     : Duration := DEFAULT_TIME_OUT;
      Max_Packets : Natural := 1) with
     Pre => Mosq.Is_Initialzed;

   not overriding
   procedure Reconnect_Delay_Set
     (Mosq                : in out Handle;
      Reconnect_Delay     : Duration := 1.0;
      Reconnect_Delay_Max : Duration := 30.0;
      Exponential_Backoff : Boolean := True) with
     Pre => Mosq.Is_Initialzed and then
     Reconnect_Delay >= 1.0 and then
     Reconnect_Delay_Max >= 1.0 and then
     Reconnect_Delay_Max > Reconnect_Delay * 2;

   Mosquitto_Error : exception;

private
   type Null_Application_Type is new Application_Interface with null record;
   overriding procedure On_Message
     (Self    : not null access Null_Application_Type;
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
      Handle      : System.Address := System.Null_Address;
      Applic      : Application_Interface_Ref := Null_Application'Access;
      Connected   : Guard_Type;
      Log_Level   : Integer := Integer'Last;
      Pw_Callback : Pw_Callback_Function;
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
