pragma Ada_2012;
with Mosquitto.Mosquitto_H;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Interfaces.C.Extensions;

package body Mosquitto is
   use Mosquitto.Mosquitto_H;
   use Interfaces.C;
   use Interfaces.C.Strings;

   package LL is

      procedure On_Connect
        (Mosq : System.Address;
         Obj  : System.Address;
         Arg3 : Int) with Convention => C;  -- /usr/include/mosquitto.h:1079

      procedure On_Disconnect
        (Mosq : System.Address;
         Obj  : System.Address;
         Arg3 : Int) with Convention => C;  -- /usr/include/mosquitto.h:1099

      procedure On_Publish
        (Mosq : System.Address;
         Obj  : System.Address;
         Arg3 : Int) with Convention => C;  -- /usr/include/mosquitto.h:1117

      procedure On_Message
        (Mosq : System.Address;
         Obj  : System.Address;
         Msg  : access constant Mosquitto_H.Mosquitto_Message) with Convention => C;  -- /usr/include/mosquitto.h:1140

      procedure On_Subscribe
        (Mosq        : System.Address;
         Obj         : System.Address;
         Mid         : Int;
         Qos_Count   : Int;
         Granted_Qos : access Int) with Convention => C;

      procedure On_Unsubscribe
        (Mosq : System.Address;
         Obj  : System.Address;
         Mid  : Int) with Convention => C;

      procedure On_Log
        (Mosq  : System.Address;
         Obj   : System.Address;
         Level : Int;
         Str   : Interfaces.C.Strings.Chars_Ptr) with Convention => C;

      function Pw_Callback
        (Buf      : Interfaces.C.Strings.Chars_Ptr;
         Size     : Int;
         Rwflag   : Int;
         Userdata : System.Address) return Int with Convention => C;
   end Ll;

   package body LL is
      function Convert is new Ada.Unchecked_Conversion (System.Address, Handle_Ref);
      procedure On_Connect
        (Mosq : System.Address;
         Obj  : System.Address;
         Arg3 : Int) is
         pragma Unreferenced (Mosq);
         This : constant Handle_Ref := Convert (Obj);
      begin
         This.Connected.Set (True);
         This.Applic.On_Connect (This, Integer (Arg3));
      end;

      procedure On_Disconnect
        (Mosq : System.Address;
         Obj  : System.Address;
         Arg3 : Int) is
         pragma Unreferenced (Mosq);
         This : constant Handle_Ref := Convert (Obj);
      begin
         This.Connected.Set (False);
         This.Applic.On_Disconnect (This, Integer (Arg3));
      end;  -- /usr/include/mosquitto.h:1099

      procedure On_Publish
        (Mosq : System.Address;
         Obj  : System.Address;
         Arg3 : Int) is
         pragma Unreferenced (Mosq);
         This : constant Handle_Ref := Convert (Obj);
      begin
         This.Applic.On_Publish (This, Message_Id (Arg3));
      end;  -- /usr/include/mosquitto.h:1117

      procedure On_Message
        (Mosq : System.Address;
         Obj  : System.Address;
         Msg  : access constant Mosquitto_H.Mosquitto_Message) is
         pragma Unreferenced (Mosq);
         This : constant Handle_Ref := Convert (Obj);
         type Buffer_Type is new Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Msg.Payloadlen));
         Buffer : Buffer_Type;
         pragma Import (C, Buffer);
         for Buffer'Address use Msg.Payload;
      begin
         This.Applic.On_Message
           (Mosq    => This,
            Mid     => Message_Id (Msg.Mid),
            Topic   => Value (Msg.Topic),
            Payload => Ada.Streams.Stream_Element_Array (Buffer),
            QoS     => QoS_Type'Val (Msg.Qos),
            Retain  => Boolean (Msg.Retain));
      end;  -- /usr/include/mosquitto.h:1140

      procedure On_Subscribe
        (Mosq        : System.Address;
         Obj         : System.Address;
         Mid         : Int;
         Qos_Count   : Int;
         Granted_Qos : access Int) is
         pragma Unreferenced (Mosq);
         This : constant Handle_Ref := Convert (Obj);

         type This_Qos_Array_Type is new Qos_Array (1 .. Natural (Qos_Count));
         Granted_Qoses : This_Qos_Array_Type;
         pragma Import (C, Granted_Qoses);
         for Granted_Qoses'Address use Granted_Qos'Address;
      begin
         This.Applic.On_Subscribe (This, Message_Id (Mid), Qos_Array (Granted_Qoses));
      end;

      procedure On_Unsubscribe
        (Mosq : System.Address;
         Obj  : System.Address;
         Mid  : Int) is
         pragma Unreferenced (Mosq);
         This : constant Handle_Ref := Convert (Obj);
      begin
         This.Applic.On_Unsubscribe (This, Message_Id (Mid));
      end;

      procedure On_Log
        (Mosq  : System.Address;
         Obj   : System.Address;
         Level : Int;
         Str   : Interfaces.C.Strings.Chars_Ptr) is
         pragma Unreferenced (Mosq);
         This : constant Handle_Ref := Convert (Obj);
      begin
         if Level > Int (This.Log_Level) then
            This.Applic.On_Log (This, Integer (Level), Value (Str));
         end if;
      end;

      function Pw_Callback
        (Buf      : Interfaces.C.Strings.Chars_Ptr;
         Size     : Int;
         Rwflag   : Int;
         Userdata : System.Address) return Int is
         pragma Unreferenced (Rwflag);
         This : constant Handle_Ref := Convert (Userdata);
      begin
         if This.Pw_Callback /= null then
            declare
               Pw : constant String := This.Pw_Callback (This.all);
            begin
               if Pw'Length > Size then
                  return -1;
               else
                  Update (Buf, 0, Pw, False);
                  return Pw'Length;
               end if;
            end;
         end if;
         return -1;
      end;
   end Ll;

   protected body Guard_Type is
      function Get return Boolean is
      begin
         return Is_Open;
      end;

      procedure Set (Open : Boolean) is
      begin
         Is_Open := Open;
      end;
      entry Wait when Is_Open is
      begin
         null;
      end;
   end Guard_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Mosq           : in out Handle;
      ID             : String;
      Clean_Sessions : Boolean := True)
   is
      L_ID : Interfaces.C.Strings.Chars_Ptr := (if Id = "" then Null_Ptr else New_String (ID));
   begin
      Mosq.Handle := Mosquitto_New (L_ID, Extensions.Bool (Clean_Sessions), Mosq'Address);
      if Mosq.Handle = System.Null_Address then
         raise Mosquitto_Error;
      end if;
      Free (L_ID);
   end Initialize;

   procedure Reinitialise (Mosq           : in out Handle;
                           ID             : String := "";
                           Clean_Sessions : Boolean := True) is
      L_ID : Interfaces.C.Strings.Chars_Ptr := (if Id = "" then Null_Ptr else New_String (ID));
      Ret  : Int;
   begin
      Ret := Mosquitto_Reinitialise (Mosq.Handle, L_ID, Extensions.Bool (Clean_Sessions), Mosq'Address);
      Free (L_ID);
      Retcode_2_Exception (Ret);
   end;

   procedure Will_Set
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : aliased String;
      Qos        : QoS_Type;
      Retain     : Boolean)
   is
   begin
      Mosq.Will_Set (Mid, Topic, Payload'Length, Payload'Address, Qos, Retain);
   end Will_Set;

   -------------
   -- will_set --
   -------------

   procedure Will_Set
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payloadlen : Message_Length;
      Payload    : System.Address;
      Qos        : QoS_Type;
      Retain     : Boolean)
   is
      L_Mid   : access Int;
      pragma Import (C, L_Mid);
      for L_Mid'Address use Mid'Address;
      L_Topic : Chars_Ptr := New_String (Topic);
      Ret     : Int;
   begin
      Ret := Mosquitto_Will_Set (Mosq       => Mosq.Handle,
                                 Topic      => L_Topic,
                                 Payloadlen => Int (Payloadlen),
                                 Payload    => Payload,
                                 Qos        => QoS_Type'Pos (Qos),
                                 Retain     => Extensions.Bool (Retain));
      Free (L_Topic);
      Retcode_2_Exception (Ret);
   end Will_Set;

   -------------
   -- will_set --
   -------------

   procedure Will_Set
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : aliased Ada.Streams.Stream_Element_Array;
      Qos        : QoS_Type;
      Retain     : Boolean)
   is
   begin
      Mosq.Will_Set (Mid, Topic, Payload'Length, Payload'Address, Qos, Retain);
   end Will_Set;

   ---------------------
   -- Publish_Generic --
   ---------------------

   procedure Will_Set_Generic (Mosq       : in out Handle;
                               Mid        : access Message_Id := null;
                               Topic      : String;
                               Payload    : aliased Msg_Type;
                               Qos        : QoS_Type;
                               Retain     : Boolean)

   is
   begin
      Mosq.Will_Set (Mid, Topic, Payload'Size / Ada.Streams.Stream_Element'Size, Payload'Address, Qos, Retain);
   end Will_Set_Generic;

   procedure Will_Clear
     (Mosq       : in out Handle) is
      Ret        : Int;
   begin
      Ret := Mosquitto_Will_Clear (Mosq.Handle);
      Retcode_2_Exception (Ret);
   end;

   procedure Username_Pw_Set (Mosq     : in out Handle;
                              Username : String;
                              Password : String := "") is
      L_Username : Interfaces.C.Strings.Chars_Ptr :=
        (if Username = ""
         then Interfaces.C.Strings.Null_Ptr
         else New_String (Username));

      L_Password : Interfaces.C.Strings.Chars_Ptr :=
        (if Password = ""
         then Interfaces.C.Strings.Null_Ptr
         else New_String (Password));
      Ret        : Int;
   begin
      Ret := Mosquitto_Username_Pw_Set (Mosq.Handle, L_Username, L_Password);
      Free (L_Username);
      Free (L_Password);
      Retcode_2_Exception (Ret);
   end;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Mosq      : in out Handle;
      Host      : String := "localhost";
      Port      : Positive := DEFAULT_PORT;
      Keepalive : Keepalive_Duration := 5.0)
   is
      L_Host     : Interfaces.C.Strings.Chars_Ptr := New_String (Host);
      Ret        : Int;
   begin
      Mosq.Connected.Set (False);
      Ret := Mosquitto_Connect
        (Mosq      => Mosq.Handle,
         Host      => L_Host,
         Port      => Int (Port),
         Keepalive => Int (Keepalive));
      Free (L_Host);
      Retcode_2_Exception (Ret);
      Mosq.Wait_Until_Connected;
   end Connect;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Mosq      : in out Handle;
      Handler   : Application_Interface_Ref)
   is
   begin
      Mosq.Applic := Handler;
      Mosquitto_Connect_Callback_Set (Mosq.Handle, Ll.On_Connect'Access);
      Mosquitto_Disconnect_Callback_Set (Mosq.Handle, Ll.On_Disconnect'Access);
      Mosquitto_Publish_Callback_Set (Mosq.Handle, Ll.On_Publish'Access);
      Mosquitto_Message_Callback_Set (Mosq.Handle, Ll.On_Message'Access);
      Mosquitto_Subscribe_Callback_Set (Mosq.Handle, Ll.On_Subscribe'Access);
      Mosquitto_Unsubscribe_Callback_Set (Mosq.Handle, Ll.On_Unsubscribe'Access);
      Mosquitto_Log_Callback_Set (Mosq.Handle, Ll.On_Log'Access);
   end Set_Handler;

   ------------------
   -- Connect_Bind --
   ------------------

   procedure Connect_Bind
     (Mosq         : in out Handle;
      Host         : String := "localhost";
      Port         : Positive := DEFAULT_PORT;
      Keepalive : Keepalive_Duration := 5.0;
      Bind_Address : String)
   is
      L_Host             : Interfaces.C.Strings.Chars_Ptr := New_String (Host);
      L_Bind_Address     : Interfaces.C.Strings.Chars_Ptr := New_String (Bind_Address);
      Ret                : Int;
   begin
      Mosq.Connected.Set (False);
      Ret := Mosquitto_Connect_Bind
        (Mosq         => Mosq.Handle,
         Host         => L_Host,
         Port         => Int (Port),
         Keepalive    => Int (Keepalive),
         Bind_Address => L_Bind_Address);
      Free (L_Host);
      Free (L_Bind_Address);
      Retcode_2_Exception (Ret);
      Mosq.Wait_Until_Connected;
   end Connect_Bind;

   -------------------
   -- Connect_Async --
   -------------------

   procedure Connect_Async
     (Mosq      : in out Handle;
      Host      : String := "localhost";
      Port      : Positive := DEFAULT_PORT;
      Keepalive : Keepalive_Duration := 5.0)
   is
      L_Host     : Interfaces.C.Strings.Chars_Ptr := New_String (Host);
      Ret        : Int;
   begin
      Mosq.Connected.Set (False);
      Ret := Mosquitto_Connect_Async
        (Mosq      => Mosq.Handle,
         Host      => L_Host,
         Port      => Int (Port),
         Keepalive => Int (Keepalive));
      Free (L_Host);
      Retcode_2_Exception (Ret);
   end Connect_Async;

   ------------------------
   -- Connect_Bind_Async --
   ------------------------

   procedure Connect_Bind_Async
     (Mosq         : in out Handle;
      Host         : String := "localhost";
      Port         : Positive := DEFAULT_PORT;
      Keepalive : Keepalive_Duration := 5.0;
      Bind_Address : String)
   is
      L_Host             : Interfaces.C.Strings.Chars_Ptr := New_String (Host);
      L_Bind_Address     : Interfaces.C.Strings.Chars_Ptr := New_String (Bind_Address);
      Ret                : Int;
   begin
      Mosq.Connected.Set (False);
      Ret := Mosquitto_Connect_Bind_Async
        (Mosq         => Mosq.Handle,
         Host         => L_Host,
         Port         => Int (Port),
         Keepalive    => Int (Keepalive),
         Bind_Address => L_Bind_Address);
      Free (L_Host);
      Free (L_Bind_Address);
      Retcode_2_Exception (Ret);
   end Connect_Bind_Async;

   -----------------
   -- Connect_Srv --
   -----------------

   procedure Connect_Srv
     (Mosq         : in out Handle;
      Host         : String := "localhost";
      Keepalive : Keepalive_Duration := 5.0;
      Bind_Address : String)
   is
      L_Host             : Interfaces.C.Strings.Chars_Ptr := New_String (Host);
      L_Bind_Address     : Interfaces.C.Strings.Chars_Ptr := New_String (Bind_Address);
      Ret                : Int;
   begin
      Mosq.Connected.Set (False);
      Ret := Mosquitto_Connect_Srv
        (Mosq         => Mosq.Handle,
         Host         => L_Host,
         Keepalive    => Int (Keepalive),
         Bind_Address => L_Bind_Address);
      Free (L_Host);
      Free (L_Bind_Address);
      Retcode_2_Exception (Ret);
      Mosq.Wait_Until_Connected;
   end Connect_Srv;

   procedure Wait_Until_Connected (Mosq : in out Handle) is
   begin
      Mosq.Connected.Wait;
   end;

   ---------------
   -- Reconnect --
   ---------------

   procedure Reconnect (Mosq : in out Handle) is
      Ret        : Int;
   begin
      Mosq.Connected.Set (False);
      Ret := Mosquitto_Reconnect
        (Mosq      => Mosq.Handle);
      Retcode_2_Exception (Ret);
   end Reconnect;

   ---------------------
   -- Reconnect_Async --
   ---------------------

   procedure Reconnect_Async (Mosq : in out Handle) is
      Ret        : Int;
   begin
      Mosq.Connected.Set (False);
      Ret := Mosquitto_Reconnect_Async
        (Mosq      => Mosq.Handle);
      Retcode_2_Exception (Ret);
   end Reconnect_Async;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Mosq : in out Handle) is
      Ret        : Int;
   begin
      Ret := Mosquitto_Disconnect
        (Mosq      => Mosq.Handle);
      Retcode_2_Exception (Ret);
   end Disconnect;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : String;
      Qos        : QoS_Type;
      Retain     : Boolean)
   is
   begin
      Mosq.Publish (Mid, Topic, Payload'Length, Payload'Address, Qos, Retain);
   end Publish;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payloadlen : Message_Length;
      Payload    : System.Address;
      Qos        : QoS_Type;
      Retain     : Boolean)
   is
      L_Mid   : access Int;
      pragma Import (C, L_Mid);
      for L_Mid'Address use Mid'Address;
      L_Topic : Chars_Ptr := New_String (Topic);
      Ret     : Int;
   begin
      Ret := Mosquitto_Publish (Mosq       => Mosq.Handle,
                                Mid        => L_Mid,
                                Topic      => L_Topic,
                                Payloadlen => Int (Payloadlen),
                                Payload    => Payload,
                                Qos        => QoS_Type'Pos (Qos),
                                Retain     => Extensions.Bool (Retain));
      Free (L_Topic);
      Retcode_2_Exception (Ret);
   end Publish;

   -------------
   -- Publish --
   -------------

   procedure Publish
     (Mosq       : in out Handle;
      Mid        : access Message_Id := null;
      Topic      : String;
      Payload    : Ada.Streams.Stream_Element_Array;
      Qos        : QoS_Type;
      Retain     : Boolean)
   is
   begin
      Mosq.Publish (Mid, Topic, Payload'Length, Payload'Address, Qos, Retain);
   end Publish;

   ---------------------
   -- Publish_Generic --
   ---------------------

   procedure Publish_Generic (Mosq       : in out Handle;
                              Mid        : access Message_Id := null;
                              Topic      : String;
                              Payload    : Msg_Type;
                              Qos        : QoS_Type;
                              Retain     : Boolean)

   is
   begin
      Mosq.Publish (Mid, Topic, Payload'Size / Ada.Streams.Stream_Element'Size, Payload'Address, Qos, Retain);
   end Publish_Generic;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe
     (Mosq  : in out Handle;
      Mid   : access Message_Id := null;
      Topic : String;
      Qos   : QoS_Type := QOS_0)
   is
      L_Mid   : access Int;
      pragma  Import (C, L_Mid);
      for L_Mid'Address use  Mid'Address;
      L_Sub   : Chars_Ptr := New_String (Topic);
      Ret     : Int;

   begin
      Ret := Mosquitto_Subscribe
        (Mosq       => Mosq.Handle,
         Mid        => L_Mid,
         Sub        => L_Sub,
         Qos        => QoS_Type'Pos (Qos));
      Free (L_Sub);
      Retcode_2_Exception (Ret);
   end Subscribe;

   -----------------
   -- UnSubscribe --
   -----------------

   procedure UnSubscribe
     (Mosq  : in out Handle;
      Mid   : access Message_Id;
      Topic : String)
   is
      L_Mid   : access Int;
      pragma Import (C, L_Mid);
      for L_Mid'Address use Mid'Address;
      L_Sub   : Chars_Ptr := New_String (Topic);
      Ret     : Int;

   begin
      Ret := Mosquitto_Unsubscribe (Mosq => Mosq.Handle,
                                    Mid  => L_Mid,
                                    Sub  => L_Sub);
      Free (L_Sub);
      Retcode_2_Exception (Ret);
   end UnSubscribe;

   procedure Tls_Set
     (Mosq        : in out Handle;
      Cafile      : String;
      Capath      : String;
      Certfile    : String;
      Keyfile     : String;
      Pw_Callback : Pw_Callback_Function) is
      Ret           : Int;
      L_Cafile      : Interfaces.C.Strings.Chars_Ptr := New_String (Cafile);
      L_Capath      : Interfaces.C.Strings.Chars_Ptr := New_String (Capath);
      L_Certfile    : Interfaces.C.Strings.Chars_Ptr := New_String (Certfile);
      L_Keyfile     : Interfaces.C.Strings.Chars_Ptr := New_String (Keyfile);

   begin
      Mosq.Pw_Callback := Pw_Callback;
      Ret := Mosquitto_Tls_Set (Mosq.Handle, L_Cafile, L_Capath, L_Certfile, L_Keyfile, LL.Pw_Callback'Access);
      Free (L_Cafile);
      Free (L_Capath);
      Free (L_Certfile);
      Free (L_Keyfile);
      Retcode_2_Exception (Ret);
   end;

   procedure Tls_Insecure_Set (Mosq : Handle; Value : Boolean) is
      Ret     : Int;
   begin
      Ret := Mosquitto_Tls_Insecure_Set (Mosq.Handle, Extensions.Bool (Value));
      Retcode_2_Exception (Ret);
   end;

   procedure Tls_Opts_Set
     (Mosq        : Handle;
      Cert_Reqs   : Verification_Mode;
      Tls_Version : String;
      Ciphers     : String) is
      Ret            : Int;
      L_Tls_Version  : Interfaces.C.Strings.Chars_Ptr := New_String (Tls_Version);
      L_Ciphers      : Interfaces.C.Strings.Chars_Ptr := New_String (Ciphers);
   begin
      Ret := Mosquitto_Tls_Opts_Set (Mosq.Handle, Verification_Mode'Pos (Cert_Reqs), L_Tls_Version, L_Ciphers);
      Free (L_Tls_Version);
      Free (L_Ciphers);
      Retcode_2_Exception (Ret);
   end;

   procedure Tls_Psk_Set
     (Mosq     : Handle;
      Psk      : String;
      Identity : String;
      Ciphers  : String) is
      Ret        : Int;
      L_Psk      : Interfaces.C.Strings.Chars_Ptr := New_String (Psk);
      L_Identity : Interfaces.C.Strings.Chars_Ptr := New_String (Identity);
      L_Ciphers  : Interfaces.C.Strings.Chars_Ptr := New_String (Ciphers);
   begin
      Ret := Mosquitto_Tls_Psk_Set (Mosq.Handle, L_Psk, L_Identity, L_Ciphers);
      Free (L_Psk);
      Free (L_Identity);
      Free (L_Ciphers);
      Retcode_2_Exception (Ret);
   end;

   procedure Do_Loop
     (Mosq        : in out Handle;
      Timeout     : Duration := DEFAULT_TIME_OUT;
      Max_Packets : Natural := 1) is
      Ret     : Int;
   begin
      Ret := Mosquitto_Loop (Mosq        => Mosq.Handle,
                             Timeout     => Int (Timeout),
                             Max_Packets => Int (Max_Packets));
      Retcode_2_Exception (Ret);
   end;

   procedure Loop_Forever
     (Mosq        : in out Handle;
      Timeout     : Duration := DEFAULT_TIME_OUT;
      Max_Packets : Natural := 1) is
      Ret     : Int;
   begin
      Ret := Mosquitto_Loop_Forever (Mosq        => Mosq.Handle,
                                     Timeout     => Int (Timeout),
                                     Max_Packets => Int (Max_Packets));
      Retcode_2_Exception (Ret);
   end;

   procedure Reconnect_Delay_Set
     (Mosq                : in out Handle;
      Reconnect_Delay     : Duration := 1.0;
      Reconnect_Delay_Max : Duration := 30.0;
      Exponential_Backoff : Boolean := True) is
      Ret     : Int;
   begin
      Ret := Mosquitto_Reconnect_Delay_Set
        (Mosq                          => Mosq.Handle,
         Reconnect_Delay               => Unsigned (Reconnect_Delay),
         Reconnect_Delay_Max           => Unsigned (Reconnect_Delay_Max),
         Reconnect_Exponential_Backoff => Extensions.Bool (Exponential_Backoff));
      Retcode_2_Exception (Ret);
   end;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Mosq : in out Handle) is
   begin
      if Mosq.Connected.Get then
         Mosq.Disconnect;
      end if;
      if Mosq.Handle /=  System.Null_Address then
         Mosquitto_Destroy (Mosq.Handle);
         Mosq.Handle := System.Null_Address;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Controler) is
      pragma Unreferenced (Object);
   begin
      Retcode_2_Exception (Mosquitto_Lib_Init);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Controler) is
      pragma Unreferenced (Object);
   begin
      Retcode_2_Exception (Mosquitto_Lib_Cleanup);
   end Finalize;

   -------------------------
   -- Retcode_2_Exception --
   -------------------------

   procedure Retcode_2_Exception (Code : Interfaces.C.Int) is
      function Img (E    : Interfaces.C.Int) return String is
         Ret : constant String := E'Img;
      begin
         return Ret (Ret'First + 1 .. Ret'Last);
      end;

   begin
      if Code /= Int (MOSQ_ERR_SUCCESS) then
         raise Mosquitto_Error with "[" & Img (Code) & "] " &  Value (Mosquitto.Mosquitto_H.Mosquitto_Strerror (Code));
      end if;
   end Retcode_2_Exception;

   Package_Initialization_Controler : Controler with Unreferenced;

   function Is_Initialzed (Mosq          : Handle) return Boolean is
   begin
      return Mosq.Handle /= System.Null_Address;
   end;

   function Is_Connected (Mosq : Handle) return Boolean is
   begin
      return Mosq.Connected.Get;
   end;

end Mosquitto;
