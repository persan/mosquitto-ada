pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with Interfaces.C.Extensions;

private package Mosquitto.Mosquitto_H is


   LIBMOSQUITTO_MAJOR    : constant := 1;  --  /usr/include/mosquitto.h:46
   LIBMOSQUITTO_MINOR    : constant := 4;  --  /usr/include/mosquitto.h:47
   LIBMOSQUITTO_REVISION : constant := 0;  --  /usr/include/mosquitto.h:48
   --  unsupported macro: LIBMOSQUITTO_VERSION_NUMBER (LIBMOSQUITTO_MAJOR*1000000+LIBMOSQUITTO_MINOR*1000+LIBMOSQUITTO_REVISION)

   MOSQ_LOG_NONE         : constant := 16#00#;  --  /usr/include/mosquitto.h:53
   MOSQ_LOG_INFO         : constant := 16#01#;  --  /usr/include/mosquitto.h:54
   MOSQ_LOG_NOTICE       : constant := 16#02#;  --  /usr/include/mosquitto.h:55
   MOSQ_LOG_WARNING      : constant := 16#04#;  --  /usr/include/mosquitto.h:56
   MOSQ_LOG_ERR          : constant := 16#08#;  --  /usr/include/mosquitto.h:57
   MOSQ_LOG_DEBUG        : constant := 16#10#;  --  /usr/include/mosquitto.h:58
   MOSQ_LOG_SUBSCRIBE    : constant := 16#20#;  --  /usr/include/mosquitto.h:59
   MOSQ_LOG_UNSUBSCRIBE  : constant := 16#40#;  --  /usr/include/mosquitto.h:60
   MOSQ_LOG_WEBSOCKETS   : constant := 16#80#;  --  /usr/include/mosquitto.h:61
   MOSQ_LOG_ALL          : constant := 16#FFFF#;  --  /usr/include/mosquitto.h:62

   MOSQ_MQTT_ID_MAX_LENGTH : constant := 23;  --  /usr/include/mosquitto.h:92

   MQTT_PROTOCOL_V31     : constant := 3;  --  /usr/include/mosquitto.h:94
   MQTT_PROTOCOL_V311    : constant := 4;  --  /usr/include/mosquitto.h:95

   --Copyright (c) 2010-2014 Roger Light <roger@atchoo.org>
   --All rights reserved. This program and the accompanying materials
   --are made available under the terms of the Eclipse Public License v1.0
   --and Eclipse Distribution License v1.0 which accompany this distribution.
   --
   --The Eclipse Public License is available at
   --   http://www.eclipse.org/legal/epl-v10.html
   --and the Eclipse Distribution License is available at
   --  http://www.eclipse.org/org/documents/edl-v10.php.
   --
   --Contributors:
   --   Roger Light - initial implementation and documentation.
   --

   -- LIBMOSQUITTO_VERSION_NUMBER looks like 1002001 for e.g. version 1.2.1.
   -- Log types
   -- Error values
   subtype Mosq_Err_T is Unsigned;
   MOSQ_ERR_CONN_PENDING : constant Mosq_Err_T := -1;
   MOSQ_ERR_SUCCESS      : constant Mosq_Err_T := 0;
   MOSQ_ERR_NOMEM        : constant Mosq_Err_T := 1;
   MOSQ_ERR_PROTOCOL     : constant Mosq_Err_T := 2;
   MOSQ_ERR_INVAL        : constant Mosq_Err_T := 3;
   MOSQ_ERR_NO_CONN      : constant Mosq_Err_T := 4;
   MOSQ_ERR_CONN_REFUSED : constant Mosq_Err_T := 5;
   MOSQ_ERR_NOT_FOUND    : constant Mosq_Err_T := 6;
   MOSQ_ERR_CONN_LOST    : constant Mosq_Err_T := 7;
   MOSQ_ERR_TLS          : constant Mosq_Err_T := 8;
   MOSQ_ERR_PAYLOAD_SIZE : constant Mosq_Err_T := 9;
   MOSQ_ERR_NOT_SUPPORTED : constant Mosq_Err_T := 10;
   MOSQ_ERR_AUTH         : constant Mosq_Err_T := 11;
   MOSQ_ERR_ACL_DENIED   : constant Mosq_Err_T := 12;
   MOSQ_ERR_UNKNOWN      : constant Mosq_Err_T := 13;
   MOSQ_ERR_ERRNO        : constant Mosq_Err_T := 14;
   MOSQ_ERR_EAI          : constant Mosq_Err_T := 15;
   MOSQ_ERR_PROXY        : constant Mosq_Err_T := 16;  -- /usr/include/mosquitto.h:65

   -- Error values
   subtype Mosq_Opt_T is Unsigned;
   MOSQ_OPT_PROTOCOL_VERSION : constant Mosq_Opt_T := 1;  -- /usr/include/mosquitto.h:87

   -- MQTT specification restricts client ids to a maximum of 23 characters
   type Mosquitto_Message is record
      Mid        : aliased Int;  -- /usr/include/mosquitto.h:98
      Topic      : Interfaces.C.Strings.Chars_Ptr;  -- /usr/include/mosquitto.h:99
      Payload    : System.Address;  -- /usr/include/mosquitto.h:100
      Payloadlen : aliased Int;  -- /usr/include/mosquitto.h:101
      Qos        : aliased Int;  -- /usr/include/mosquitto.h:102
      Retain     : aliased Extensions.Bool;  -- /usr/include/mosquitto.h:103
   end record;
   pragma Convention (C_Pass_By_Copy, Mosquitto_Message);  -- /usr/include/mosquitto.h:97

   --  skipped empty struct mosquitto

   -- * Topic: Threads
   -- *	libmosquitto provides thread safe operation, with the exception of
   -- *	<mosquitto_lib_init> which is not thread safe.
   -- *
   -- *	If your application uses threads you must use <mosquitto_threaded_set> to
   -- *	tell the library this is the case, otherwise it makes some optimisations
   -- *	for the single threaded case that may result in unexpected behaviour for
   -- *	the multi threaded case.
   --

   --**************************************************
   -- * Important note
   -- *
   -- * The following functions that deal with network operations will return
   -- * MOSQ_ERR_SUCCESS on success, but this does not mean that the operation has
   -- * taken place. An attempt will be made to write the network data, but if the
   -- * socket is not available for writing at that time then the packet will not be
   -- * sent. To ensure the packet is sent, call mosquitto_loop() (which must also
   -- * be called to process incoming network data).
   -- * This is especially important when disconnecting a client that has a will. If
   -- * the broker does not receive the DISCONNECT command, it will assume that the
   -- * client has disconnected unexpectedly and send the will.
   -- *
   -- * mosquitto_connect()
   -- * mosquitto_disconnect()
   -- * mosquitto_subscribe()
   -- * mosquitto_unsubscribe()
   -- * mosquitto_publish()
   -- **************************************************

   -- * Function: mosquitto_lib_version
   -- *
   -- * Can be used to obtain version information for the mosquitto library.
   -- * This allows the application to compare the library version against the
   -- * version it was compiled against by using the LIBMOSQUITTO_MAJOR,
   -- * LIBMOSQUITTO_MINOR and LIBMOSQUITTO_REVISION defines.
   -- *
   -- * Parameters:
   -- *  major -    an integer pointer. If not NULL, the major version of the
   -- *             library will be returned in this variable.
   -- *  minor -    an integer pointer. If not NULL, the minor version of the
   -- *             library will be returned in this variable.
   -- *  revision - an integer pointer. If not NULL, the revision of the library will
   -- *             be returned in this variable.
   -- *
   -- * Returns:
   -- *	LIBMOSQUITTO_VERSION_NUMBER, which is a unique number based on the major,
   -- *		minor and revision values.
   -- * See Also:
   -- * 	<mosquitto_lib_cleanup>, <mosquitto_lib_init>
   --

   function Mosquitto_Lib_Version
     (Major    : access Int;
      Minor    : access Int;
      Revision : access Int) return Int;  -- /usr/include/mosquitto.h:160
   pragma Import (C, Mosquitto_Lib_Version, "mosquitto_lib_version");

   -- * Function: mosquitto_lib_init
   -- *
   -- * Must be called before any other mosquitto functions.
   -- *
   -- * This function is *not* thread safe.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - always
   -- *
   -- * See Also:
   -- * 	<mosquitto_lib_cleanup>, <mosquitto_lib_version>
   --

   function Mosquitto_Lib_Init return Int;  -- /usr/include/mosquitto.h:175
   pragma Import (C, Mosquitto_Lib_Init, "mosquitto_lib_init");

   -- * Function: mosquitto_lib_cleanup
   -- *
   -- * Call to free resources associated with the library.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - always
   -- *
   -- * See Also:
   -- * 	<mosquitto_lib_init>, <mosquitto_lib_version>
   --

   function Mosquitto_Lib_Cleanup return Int;  -- /usr/include/mosquitto.h:188
   pragma Import (C, Mosquitto_Lib_Cleanup, "mosquitto_lib_cleanup");

   -- * Function: mosquitto_new
   -- *
   -- * Create a new mosquitto client instance.
   -- *
   -- * Parameters:
   -- * 	id -            String to use as the client id. If NULL, a random client id
   -- * 	                will be generated. If id is NULL, clean_session must be true.
   -- * 	clean_session - set to true to instruct the broker to clean all messages
   -- *                  and subscriptions on disconnect, false to instruct it to
   -- *                  keep them. See the man page mqtt(7) for more details.
   -- *                  Note that a client will never discard its own outgoing
   -- *                  messages on disconnect. Calling <mosquitto_connect> or
   -- *                  <mosquitto_reconnect> will cause the messages to be resent.
   -- *                  Use <mosquitto_reinitialise> to reset a client to its
   -- *                  original state.
   -- *                  Must be set to true if the id parameter is NULL.
   -- * 	obj -           A user pointer that will be passed as an argument to any
   -- *                  callbacks that are specified.
   -- *
   -- * Returns:
   -- * 	Pointer to a struct mosquitto on success.
   -- * 	NULL on failure. Interrogate errno to determine the cause for the failure:
   -- *      - ENOMEM on out of memory.
   -- *      - EINVAL on invalid input parameters.
   -- *
   -- * See Also:
   -- * 	<mosquitto_reinitialise>, <mosquitto_destroy>, <mosquitto_user_data_set>
   --

   function Mosquitto_New
     (ID            : Interfaces.C.Strings.Chars_Ptr;
      Clean_Session : Extensions.Bool;
      Obj           : System.Address) return System.Address;  -- /usr/include/mosquitto.h:219
   pragma Import (C, Mosquitto_New, "mosquitto_new");

   --
   -- * Function: mosquitto_destroy
   -- *
   -- * Use to free memory associated with a mosquitto client instance.
   -- *
   -- * Parameters:
   -- * 	mosq - a struct mosquitto pointer to free.
   -- *
   -- * See Also:
   -- * 	<mosquitto_new>, <mosquitto_reinitialise>
   --

   procedure Mosquitto_Destroy (Mosq : System.Address);  -- /usr/include/mosquitto.h:232
   pragma Import (C, Mosquitto_Destroy, "mosquitto_destroy");

   -- * Function: mosquitto_reinitialise
   -- *
   -- * This function allows an existing mosquitto client to be reused. Call on a
   -- * mosquitto instance to close any open network connections, free memory
   -- * and reinitialise the client with the new parameters. The end result is the
   -- * same as the output of <mosquitto_new>.
   -- *
   -- * Parameters:
   -- * 	mosq -          a valid mosquitto instance.
   -- * 	id -            string to use as the client id. If NULL, a random client id
   -- * 	                will be generated. If id is NULL, clean_session must be true.
   -- * 	clean_session - set to true to instruct the broker to clean all messages
   -- *                  and subscriptions on disconnect, false to instruct it to
   -- *                  keep them. See the man page mqtt(7) for more details.
   -- *                  Must be set to true if the id parameter is NULL.
   -- * 	obj -           A user pointer that will be passed as an argument to any
   -- *                  callbacks that are specified.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * See Also:
   -- * 	<mosquitto_new>, <mosquitto_destroy>
   --

   function Mosquitto_Reinitialise
     (Mosq          : System.Address;
      ID            : Interfaces.C.Strings.Chars_Ptr;
      Clean_Session : Extensions.Bool;
      Obj           : System.Address) return Int;  -- /usr/include/mosquitto.h:261
   pragma Import (C, Mosquitto_Reinitialise, "mosquitto_reinitialise");

   --
   -- * Function: mosquitto_will_set
   -- *
   -- * Configure will information for a mosquitto instance. By default, clients do
   -- * not have a will.  This must be called before calling <mosquitto_connect>.
   -- *
   -- * Parameters:
   -- * 	mosq -       a valid mosquitto instance.
   -- * 	topic -      the topic on which to publish the will.
   -- * 	payloadlen - the size of the payload (bytes). Valid values are between 0 and
   -- *               268,435,455.
   -- * 	payload -    pointer to the data to send. If payloadlen > 0 this must be a
   -- *               valid memory location.
   -- * 	qos -        integer value 0, 1 or 2 indicating the Quality of Service to be
   -- *               used for the will.
   -- * 	retain -     set to true to make the will a retained message.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS -      on success.
   -- * 	MOSQ_ERR_INVAL -        if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -        if an out of memory condition occurred.
   -- * 	MOSQ_ERR_PAYLOAD_SIZE - if payloadlen is too large.
   --

   function Mosquitto_Will_Set
     (Mosq       : System.Address;
      Topic      : Interfaces.C.Strings.Chars_Ptr;
      Payloadlen : Int;
      Payload    : System.Address;
      Qos        : Int;
      Retain     : Extensions.Bool) return Int;  -- /usr/include/mosquitto.h:286
   pragma Import (C, Mosquitto_Will_Set, "mosquitto_will_set");

   --
   -- * Function: mosquitto_will_clear
   -- *
   -- * Remove a previously configured will. This must be called before calling
   -- * <mosquitto_connect>.
   -- *
   -- * Parameters:
   -- * 	mosq - a valid mosquitto instance.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   --

   function Mosquitto_Will_Clear (Mosq : System.Address) return Int;  -- /usr/include/mosquitto.h:301
   pragma Import (C, Mosquitto_Will_Clear, "mosquitto_will_clear");

   -- * Function: mosquitto_username_pw_set
   -- *
   -- * Configure username and password for a mosquitton instance. This is only
   -- * supported by brokers that implement the MQTT spec v3.1. By default, no
   -- * username or password will be sent.
   -- * If username is NULL, the password argument is ignored.
   -- * This must be called before calling mosquitto_connect().
   -- *
   -- * This is must be called before calling <mosquitto_connect>.
   -- *
   -- * Parameters:
   -- * 	mosq -     a valid mosquitto instance.
   -- * 	username - the username to send as a string, or NULL to disable
   -- *             authentication.
   -- * 	password - the password to send as a string. Set to NULL when username is
   -- * 	           valid in order to send just a username.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   --

   function Mosquitto_Username_Pw_Set
     (Mosq     : System.Address;
      Username : Interfaces.C.Strings.Chars_Ptr;
      Password : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:326
   pragma Import (C, Mosquitto_Username_Pw_Set, "mosquitto_username_pw_set");

   -- * Function: mosquitto_connect
   -- *
   -- * Connect to an MQTT broker.
   -- *
   -- * Parameters:
   -- * 	mosq -      a valid mosquitto instance.
   -- * 	host -      the hostname or ip address of the broker to connect to.
   -- * 	port -      the network port to connect to. Usually 1883.
   -- * 	keepalive - the number of seconds after which the broker should send a PING
   -- *              message to the client if no other messages have been exchanged
   -- *              in that time.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_ERRNO -   if a system call returned an error. The variable errno
   -- *                     contains the error code, even on Windows.
   -- *                     Use strerror_r() where available or FormatMessage() on
   -- *                     Windows.
   -- *
   -- * See Also:
   -- * 	<mosquitto_connect_bind>, <mosquitto_connect_async>, <mosquitto_reconnect>, <mosquitto_disconnect>, <mosquitto_tls_set>
   --

   function Mosquitto_Connect
     (Mosq      : System.Address;
      Host      : Interfaces.C.Strings.Chars_Ptr;
      Port      : Int;
      Keepalive : Int) return Int;  -- /usr/include/mosquitto.h:352
   pragma Import (C, Mosquitto_Connect, "mosquitto_connect");

   -- * Function: mosquitto_connect_bind
   -- *
   -- * Connect to an MQTT broker. This extends the functionality of
   -- * <mosquitto_connect> by adding the bind_address parameter. Use this function
   -- * if you need to restrict network communication over a particular interface.
   -- *
   -- * Parameters:
   -- * 	mosq -         a valid mosquitto instance.
   -- * 	host -         the hostname or ip address of the broker to connect to.
   -- * 	port -         the network port to connect to. Usually 1883.
   -- * 	keepalive -    the number of seconds after which the broker should send a PING
   -- *                 message to the client if no other messages have been exchanged
   -- *                 in that time.
   -- *  bind_address - the hostname or ip address of the local network interface to
   -- *                 bind to.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_ERRNO -   if a system call returned an error. The variable errno
   -- *                     contains the error code, even on Windows.
   -- *                     Use strerror_r() where available or FormatMessage() on
   -- *                     Windows.
   -- *
   -- * See Also:
   -- * 	<mosquitto_connect>, <mosquitto_connect_async>, <mosquitto_connect_bind_async>
   --

   function Mosquitto_Connect_Bind
     (Mosq         : System.Address;
      Host         : Interfaces.C.Strings.Chars_Ptr;
      Port         : Int;
      Keepalive    : Int;
      Bind_Address : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:382
   pragma Import (C, Mosquitto_Connect_Bind, "mosquitto_connect_bind");

   -- * Function: mosquitto_connect_async
   -- *
   -- * Connect to an MQTT broker. This is a non-blocking call. If you use
   -- * <mosquitto_connect_async> your client must use the threaded interface
   -- * <mosquitto_loop_start>. If you need to use <mosquitto_loop>, you must use
   -- * <mosquitto_connect> to connect the client.
   -- *
   -- * May be called before or after <mosquitto_loop_start>.
   -- *
   -- * Parameters:
   -- * 	mosq -      a valid mosquitto instance.
   -- * 	host -      the hostname or ip address of the broker to connect to.
   -- * 	port -      the network port to connect to. Usually 1883.
   -- * 	keepalive - the number of seconds after which the broker should send a PING
   -- *              message to the client if no other messages have been exchanged
   -- *              in that time.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_ERRNO -   if a system call returned an error. The variable errno
   -- *                     contains the error code, even on Windows.
   -- *                     Use strerror_r() where available or FormatMessage() on
   -- *                     Windows.
   -- *
   -- * See Also:
   -- * 	<mosquitto_connect_bind_async>, <mosquitto_connect>, <mosquitto_reconnect>, <mosquitto_disconnect>, <mosquitto_tls_set>
   --

   function Mosquitto_Connect_Async
     (Mosq      : System.Address;
      Host      : Interfaces.C.Strings.Chars_Ptr;
      Port      : Int;
      Keepalive : Int) return Int;  -- /usr/include/mosquitto.h:413
   pragma Import (C, Mosquitto_Connect_Async, "mosquitto_connect_async");

   -- * Function: mosquitto_connect_bind_async
   -- *
   -- * Connect to an MQTT broker. This is a non-blocking call. If you use
   -- * <mosquitto_connect_bind_async> your client must use the threaded interface
   -- * <mosquitto_loop_start>. If you need to use <mosquitto_loop>, you must use
   -- * <mosquitto_connect> to connect the client.
   -- *
   -- * This extends the functionality of <mosquitto_connect_async> by adding the
   -- * bind_address parameter. Use this function if you need to restrict network
   -- * communication over a particular interface.
   -- *
   -- * May be called before or after <mosquitto_loop_start>.
   -- *
   -- * Parameters:
   -- * 	mosq -         a valid mosquitto instance.
   -- * 	host -         the hostname or ip address of the broker to connect to.
   -- * 	port -         the network port to connect to. Usually 1883.
   -- * 	keepalive -    the number of seconds after which the broker should send a PING
   -- *                 message to the client if no other messages have been exchanged
   -- *                 in that time.
   -- *  bind_address - the hostname or ip address of the local network interface to
   -- *                 bind to.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_ERRNO -   if a system call returned an error. The variable errno
   -- *                     contains the error code, even on Windows.
   -- *                     Use strerror_r() where available or FormatMessage() on
   -- *                     Windows.
   -- *
   -- * See Also:
   -- * 	<mosquitto_connect_async>, <mosquitto_connect>, <mosquitto_connect_bind>
   --

   function Mosquitto_Connect_Bind_Async
     (Mosq         : System.Address;
      Host         : Interfaces.C.Strings.Chars_Ptr;
      Port         : Int;
      Keepalive    : Int;
      Bind_Address : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:450
   pragma Import (C, Mosquitto_Connect_Bind_Async, "mosquitto_connect_bind_async");

   -- * Function: mosquitto_connect_srv
   -- *
   -- * Connect to an MQTT broker. This is a non-blocking call. If you use
   -- * <mosquitto_connect_async> your client must use the threaded interface
   -- * <mosquitto_loop_start>. If you need to use <mosquitto_loop>, you must use
   -- * <mosquitto_connect> to connect the client.
   -- *
   -- * This extends the functionality of <mosquitto_connect_async> by adding the
   -- * bind_address parameter. Use this function if you need to restrict network
   -- * communication over a particular interface.
   -- *
   -- * May be called before or after <mosquitto_loop_start>.
   -- *
   -- * Parameters:
   -- * 	mosq -         a valid mosquitto instance.
   -- * 	host -         the hostname or ip address of the broker to connect to.
   -- * 	keepalive -    the number of seconds after which the broker should send a PING
   -- *                 message to the client if no other messages have been exchanged
   -- *                 in that time.
   -- *  bind_address - the hostname or ip address of the local network interface to
   -- *                 bind to.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_ERRNO -   if a system call returned an error. The variable errno
   -- *                     contains the error code, even on Windows.
   -- *                     Use strerror_r() where available or FormatMessage() on
   -- *                     Windows.
   -- *
   -- * See Also:
   -- * 	<mosquitto_connect_async>, <mosquitto_connect>, <mosquitto_connect_bind>
   --

   function Mosquitto_Connect_Srv
     (Mosq         : System.Address;
      Host         : Interfaces.C.Strings.Chars_Ptr;
      Keepalive    : Int;
      Bind_Address : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:486
   pragma Import (C, Mosquitto_Connect_Srv, "mosquitto_connect_srv");

   -- * Function: mosquitto_reconnect
   -- *
   -- * Reconnect to a broker.
   -- *
   -- * This function provides an easy way of reconnecting to a broker after a
   -- * connection has been lost. It uses the values that were provided in the
   -- * <mosquitto_connect> call. It must not be called before
   -- * <mosquitto_connect>.
   -- *
   -- * Parameters:
   -- * 	mosq - a valid mosquitto instance.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_ERRNO -   if a system call returned an error. The variable errno
   -- *                     contains the error code, even on Windows.
   -- *                     Use strerror_r() where available or FormatMessage() on
   -- *                     Windows.
   -- *
   -- * See Also:
   -- * 	<mosquitto_connect>, <mosquitto_disconnect>, <mosquitto_reconnect_async>
   --

   function Mosquitto_Reconnect (Mosq : System.Address) return Int;  -- /usr/include/mosquitto.h:517
   pragma Import (C, Mosquitto_Reconnect, "mosquitto_reconnect");

   -- * Function: mosquitto_reconnect_async
   -- *
   -- * Reconnect to a broker. Non blocking version of <mosquitto_reconnect>.
   -- *
   -- * This function provides an easy way of reconnecting to a broker after a
   -- * connection has been lost. It uses the values that were provided in the
   -- * <mosquitto_connect> or <mosquitto_connect_async> calls. It must not be
   -- * called before <mosquitto_connect>.
   -- *
   -- * Parameters:
   -- * 	mosq - a valid mosquitto instance.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_ERRNO -   if a system call returned an error. The variable errno
   -- *                     contains the error code, even on Windows.
   -- *                     Use strerror_r() where available or FormatMessage() on
   -- *                     Windows.
   -- *
   -- * See Also:
   -- * 	<mosquitto_connect>, <mosquitto_disconnect>
   --

   function Mosquitto_Reconnect_Async (Mosq : System.Address) return Int;  -- /usr/include/mosquitto.h:548
   pragma Import (C, Mosquitto_Reconnect_Async, "mosquitto_reconnect_async");

   -- * Function: mosquitto_disconnect
   -- *
   -- * Disconnect from the broker.
   -- *
   -- * Parameters:
   -- *	mosq - a valid mosquitto instance.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NO_CONN -  if the client isn't connected to a broker.
   --

   function Mosquitto_Disconnect (Mosq : System.Address) return Int;  -- /usr/include/mosquitto.h:563
   pragma Import (C, Mosquitto_Disconnect, "mosquitto_disconnect");

   --
   -- * Function: mosquitto_publish
   -- *
   -- * Publish a message on a given topic.
   -- *
   -- * Parameters:
   -- * 	mosq -       a valid mosquitto instance.
   -- * 	mid -        pointer to an int. If not NULL, the function will set this
   -- *               to the message id of this particular message. This can be then
   -- *               used with the publish callback to determine when the message
   -- *               has been sent.
   -- *               Note that although the MQTT protocol doesn't use message ids
   -- *               for messages with QoS=0, libmosquitto assigns them message ids
   -- *               so they can be tracked with this parameter.
   -- * 	payloadlen - the size of the payload (bytes). Valid values are between 0 and
   -- *               268,435,455.
   -- * 	payload -    pointer to the data to send. If payloadlen > 0 this must be a
   -- *               valid memory location.
   -- * 	qos -        integer value 0, 1 or 2 indicating the Quality of Service to be
   -- *               used for the message.
   -- * 	retain -     set to true to make the message retained.
   -- *
   -- * Returns:
   -- * 	MOSQ_ERR_SUCCESS -      on success.
   -- * 	MOSQ_ERR_INVAL -        if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -        if an out of memory condition occurred.
   -- * 	MOSQ_ERR_NO_CONN -      if the client isn't connected to a broker.
   -- *	MOSQ_ERR_PROTOCOL -     if there is a protocol error communicating with the
   -- *                          broker.
   -- * 	MOSQ_ERR_PAYLOAD_SIZE - if payloadlen is too large.
   -- *
   -- * See Also:
   -- *	<mosquitto_max_inflight_messages_set>
   --

   function Mosquitto_Publish
     (Mosq       : System.Address;
      Mid        : access Int;
      Topic      : Interfaces.C.Strings.Chars_Ptr;
      Payloadlen : Int;
      Payload    : System.Address;
      Qos        : Int;
      Retain     : Extensions.Bool) return Int;  -- /usr/include/mosquitto.h:599
   pragma Import (C, Mosquitto_Publish, "mosquitto_publish");

   -- * Function: mosquitto_subscribe
   -- *
   -- * Subscribe to a topic.
   -- *
   -- * Parameters:
   -- *	mosq - a valid mosquitto instance.
   -- *	mid -  a pointer to an int. If not NULL, the function will set this to
   -- *	       the message id of this particular message. This can be then used
   -- *	       with the subscribe callback to determine when the message has been
   -- *	       sent.
   -- *	sub -  the subscription pattern.
   -- *	qos -  the requested Quality of Service for this subscription.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- * 	MOSQ_ERR_NO_CONN - if the client isn't connected to a broker.
   --

   function Mosquitto_Subscribe
     (Mosq : System.Address;
      Mid  : access Int;
      Sub  : Interfaces.C.Strings.Chars_Ptr;
      Qos  : Int) return Int;  -- /usr/include/mosquitto.h:621
   pragma Import (C, Mosquitto_Subscribe, "mosquitto_subscribe");

   -- * Function: mosquitto_unsubscribe
   -- *
   -- * Unsubscribe from a topic.
   -- *
   -- * Parameters:
   -- *	mosq - a valid mosquitto instance.
   -- *	mid -  a pointer to an int. If not NULL, the function will set this to
   -- *	       the message id of this particular message. This can be then used
   -- *	       with the unsubscribe callback to determine when the message has been
   -- *	       sent.
   -- *	sub -  the unsubscription pattern.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- * 	MOSQ_ERR_NO_CONN - if the client isn't connected to a broker.
   --

   function Mosquitto_Unsubscribe
     (Mosq : System.Address;
      Mid  : access Int;
      Sub  : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:642
   pragma Import (C, Mosquitto_Unsubscribe, "mosquitto_unsubscribe");

   -- * Function: mosquitto_message_copy
   -- *
   -- * Copy the contents of a mosquitto message to another message.
   -- * Useful for preserving a message received in the on_message() callback.
   -- *
   -- * Parameters:
   -- *	dst - a pointer to a valid mosquitto_message struct to copy to.
   -- *	src - a pointer to a valid mosquitto_message struct to copy from.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * See Also:
   -- * 	<mosquitto_message_free>
   --

   function Mosquitto_Message_Copy (Dst : access Mosquitto_Message; Src : access constant Mosquitto_Message) return Int;  -- /usr/include/mosquitto.h:662
   pragma Import (C, Mosquitto_Message_Copy, "mosquitto_message_copy");

   -- * Function: mosquitto_message_free
   -- *
   -- * Completely free a mosquitto_message struct.
   -- *
   -- * Parameters:
   -- *	message - pointer to a mosquitto_message pointer to free.
   -- *
   -- * See Also:
   -- * 	<mosquitto_message_copy>
   --

   procedure Mosquitto_Message_Free (Message : System.Address);  -- /usr/include/mosquitto.h:675
   pragma Import (C, Mosquitto_Message_Free, "mosquitto_message_free");

   -- * Function: mosquitto_loop
   -- *
   -- * The main network loop for the client. You must call this frequently in order
   -- * to keep communications between the client and broker working. If incoming
   -- * data is present it will then be processed. Outgoing commands, from e.g.
   -- * <mosquitto_publish>, are normally sent immediately that their function is
   -- * called, but this is not always possible. <mosquitto_loop> will also attempt
   -- * to send any remaining outgoing messages, which also includes commands that
   -- * are part of the flow for messages with QoS>0.
   -- *
   -- * An alternative approach is to use <mosquitto_loop_start> to run the client
   -- * loop in its own thread.
   -- *
   -- * This calls select() to monitor the client network socket. If you want to
   -- * integrate mosquitto client operation with your own select() call, use
   -- * <mosquitto_socket>, <mosquitto_loop_read>, <mosquitto_loop_write> and
   -- * <mosquitto_loop_misc>.
   -- *
   -- * Threads:
   -- *
   -- * Parameters:
   -- *	mosq -        a valid mosquitto instance.
   -- *	timeout -     Maximum number of milliseconds to wait for network activity
   -- *	              in the select() call before timing out. Set to 0 for instant
   -- *	              return.  Set negative to use the default of 1000ms.
   -- *	max_packets - this parameter is currently unused and should be set to 1 for
   -- *	              future compatibility.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS -   on success.
   -- * 	MOSQ_ERR_INVAL -     if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -     if an out of memory condition occurred.
   -- * 	MOSQ_ERR_NO_CONN -   if the client isn't connected to a broker.
   -- *  MOSQ_ERR_CONN_LOST - if the connection to the broker was lost.
   -- *	MOSQ_ERR_PROTOCOL -  if there is a protocol error communicating with the
   -- *                       broker.
   -- * 	MOSQ_ERR_ERRNO -     if a system call returned an error. The variable errno
   -- *                       contains the error code, even on Windows.
   -- *                       Use strerror_r() where available or FormatMessage() on
   -- *                       Windows.
   -- * See Also:
   -- *	<mosquitto_loop_forever>, <mosquitto_loop_start>, <mosquitto_loop_stop>
   --

   function Mosquitto_Loop
     (Mosq        : System.Address;
      Timeout     : Int;
      Max_Packets : Int) return Int;  -- /usr/include/mosquitto.h:721
   pragma Import (C, Mosquitto_Loop, "mosquitto_loop");

   -- * Function: mosquitto_loop_forever
   -- *
   -- * This function call loop() for you in an infinite blocking loop. It is useful
   -- * for the case where you only want to run the MQTT client loop in your
   -- * program.
   -- *
   -- * It handles reconnecting in case server connection is lost. If you call
   -- * mosquitto_disconnect() in a callback it will return.
   -- *
   -- * Parameters:
   -- *  mosq - a valid mosquitto instance.
   -- *	timeout -     Maximum number of milliseconds to wait for network activity
   -- *	              in the select() call before timing out. Set to 0 for instant
   -- *	              return.  Set negative to use the default of 1000ms.
   -- *	max_packets - this parameter is currently unused and should be set to 1 for
   -- *	              future compatibility.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS -   on success.
   -- * 	MOSQ_ERR_INVAL -     if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -     if an out of memory condition occurred.
   -- * 	MOSQ_ERR_NO_CONN -   if the client isn't connected to a broker.
   -- *  MOSQ_ERR_CONN_LOST - if the connection to the broker was lost.
   -- *	MOSQ_ERR_PROTOCOL -  if there is a protocol error communicating with the
   -- *                       broker.
   -- * 	MOSQ_ERR_ERRNO -     if a system call returned an error. The variable errno
   -- *                       contains the error code, even on Windows.
   -- *                       Use strerror_r() where available or FormatMessage() on
   -- *                       Windows.
   -- *
   -- * See Also:
   -- *	<mosquitto_loop>, <mosquitto_loop_start>
   --

   function Mosquitto_Loop_Forever
     (Mosq        : System.Address;
      Timeout     : Int;
      Max_Packets : Int) return Int;  -- /usr/include/mosquitto.h:757
   pragma Import (C, Mosquitto_Loop_Forever, "mosquitto_loop_forever");

   -- * Function: mosquitto_loop_start
   -- *
   -- * This is part of the threaded client interface. Call this once to start a new
   -- * thread to process network traffic. This provides an alternative to
   -- * repeatedly calling <mosquitto_loop> yourself.
   -- *
   -- * Parameters:
   -- *  mosq - a valid mosquitto instance.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS -       on success.
   -- * 	MOSQ_ERR_INVAL -         if the input parameters were invalid.
   -- *	MOSQ_ERR_NOT_SUPPORTED - if thread support is not available.
   -- *
   -- * See Also:
   -- *	<mosquitto_connect_async>, <mosquitto_loop>, <mosquitto_loop_forever>, <mosquitto_loop_stop>
   --

   function Mosquitto_Loop_Start (Mosq : System.Address) return Int;  -- /usr/include/mosquitto.h:777
   pragma Import (C, Mosquitto_Loop_Start, "mosquitto_loop_start");

   -- * Function: mosquitto_loop_stop
   -- *
   -- * This is part of the threaded client interface. Call this once to stop the
   -- * network thread previously created with <mosquitto_loop_start>. This call
   -- * will block until the network thread finishes. For the network thread to end,
   -- * you must have previously called <mosquitto_disconnect> or have set the force
   -- * parameter to true.
   -- *
   -- * Parameters:
   -- *  mosq - a valid mosquitto instance.
   -- *	force - set to true to force thread cancellation. If false,
   -- *	        <mosquitto_disconnect> must have already been called.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS -       on success.
   -- * 	MOSQ_ERR_INVAL -         if the input parameters were invalid.
   -- *	MOSQ_ERR_NOT_SUPPORTED - if thread support is not available.
   -- *
   -- * See Also:
   -- *	<mosquitto_loop>, <mosquitto_loop_start>
   --

   function Mosquitto_Loop_Stop (Mosq : System.Address; Force : Extensions.Bool) return Int;  -- /usr/include/mosquitto.h:801
   pragma Import (C, Mosquitto_Loop_Stop, "mosquitto_loop_stop");

   -- * Function: mosquitto_socket
   -- *
   -- * Return the socket handle for a mosquitto instance. Useful if you want to
   -- * include a mosquitto client in your own select() calls.
   -- *
   -- * Parameters:
   -- *	mosq - a valid mosquitto instance.
   -- *
   -- * Returns:
   -- *	The socket for the mosquitto client or -1 on failure.
   --

   function Mosquitto_Socket (Mosq : System.Address) return Int;  -- /usr/include/mosquitto.h:815
   pragma Import (C, Mosquitto_Socket, "mosquitto_socket");

   -- * Function: mosquitto_loop_read
   -- *
   -- * Carry out network read operations.
   -- * This should only be used if you are not using mosquitto_loop() and are
   -- * monitoring the client network socket for activity yourself.
   -- *
   -- * Parameters:
   -- *	mosq -        a valid mosquitto instance.
   -- *	max_packets - this parameter is currently unused and should be set to 1 for
   -- *	              future compatibility.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS -   on success.
   -- * 	MOSQ_ERR_INVAL -     if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -     if an out of memory condition occurred.
   -- * 	MOSQ_ERR_NO_CONN -   if the client isn't connected to a broker.
   -- *  MOSQ_ERR_CONN_LOST - if the connection to the broker was lost.
   -- *	MOSQ_ERR_PROTOCOL -  if there is a protocol error communicating with the
   -- *                       broker.
   -- * 	MOSQ_ERR_ERRNO -     if a system call returned an error. The variable errno
   -- *                       contains the error code, even on Windows.
   -- *                       Use strerror_r() where available or FormatMessage() on
   -- *                       Windows.
   -- *
   -- * See Also:
   -- *	<mosquitto_socket>, <mosquitto_loop_write>, <mosquitto_loop_misc>
   --

   function Mosquitto_Loop_Read (Mosq : System.Address; Max_Packets : Int) return Int;  -- /usr/include/mosquitto.h:845
   pragma Import (C, Mosquitto_Loop_Read, "mosquitto_loop_read");

   -- * Function: mosquitto_loop_write
   -- *
   -- * Carry out network write operations.
   -- * This should only be used if you are not using mosquitto_loop() and are
   -- * monitoring the client network socket for activity yourself.
   -- *
   -- * Parameters:
   -- *	mosq -        a valid mosquitto instance.
   -- *	max_packets - this parameter is currently unused and should be set to 1 for
   -- *	              future compatibility.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS -   on success.
   -- * 	MOSQ_ERR_INVAL -     if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -     if an out of memory condition occurred.
   -- * 	MOSQ_ERR_NO_CONN -   if the client isn't connected to a broker.
   -- *  MOSQ_ERR_CONN_LOST - if the connection to the broker was lost.
   -- *	MOSQ_ERR_PROTOCOL -  if there is a protocol error communicating with the
   -- *                       broker.
   -- * 	MOSQ_ERR_ERRNO -     if a system call returned an error. The variable errno
   -- *                       contains the error code, even on Windows.
   -- *                       Use strerror_r() where available or FormatMessage() on
   -- *                       Windows.
   -- *
   -- * See Also:
   -- *	<mosquitto_socket>, <mosquitto_loop_read>, <mosquitto_loop_misc>, <mosquitto_want_write>
   --

   function Mosquitto_Loop_Write (Mosq : System.Address; Max_Packets : Int) return Int;  -- /usr/include/mosquitto.h:875
   pragma Import (C, Mosquitto_Loop_Write, "mosquitto_loop_write");

   -- * Function: mosquitto_loop_misc
   -- *
   -- * Carry out miscellaneous operations required as part of the network loop.
   -- * This should only be used if you are not using mosquitto_loop() and are
   -- * monitoring the client network socket for activity yourself.
   -- *
   -- * This function deals with handling PINGs and checking whether messages need
   -- * to be retried, so should be called fairly frequently.
   -- *
   -- * Parameters:
   -- *	mosq - a valid mosquitto instance.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS -   on success.
   -- * 	MOSQ_ERR_INVAL -     if the input parameters were invalid.
   -- * 	MOSQ_ERR_NO_CONN -   if the client isn't connected to a broker.
   -- *
   -- * See Also:
   -- *	<mosquitto_socket>, <mosquitto_loop_read>, <mosquitto_loop_write>
   --

   function Mosquitto_Loop_Misc (Mosq : System.Address) return Int;  -- /usr/include/mosquitto.h:898
   pragma Import (C, Mosquitto_Loop_Misc, "mosquitto_loop_misc");

   -- * Function: mosquitto_want_write
   -- *
   -- * Returns true if there is data ready to be written on the socket.
   -- *
   -- * Parameters:
   -- *	mosq - a valid mosquitto instance.
   -- *
   -- * See Also:
   -- *	<mosquitto_socket>, <mosquitto_loop_read>, <mosquitto_loop_write>
   --

   function Mosquitto_Want_Write (Mosq : System.Address) return Extensions.Bool;  -- /usr/include/mosquitto.h:911
   pragma Import (C, Mosquitto_Want_Write, "mosquitto_want_write");

   -- * Function: mosquitto_threaded_set
   -- *
   -- * Used to tell the library that your application is using threads, but not
   -- * using <mosquitto_loop_start>. The library operates slightly differently when
   -- * not in threaded mode in order to simplify its operation. If you are managing
   -- * your own threads and do not use this function you will experience crashes
   -- * due to race conditions.
   -- *
   -- * When using <mosquitto_loop_start>, this is set automatically.
   -- *
   -- * Parameters:
   -- *  mosq -     a valid mosquitto instance.
   -- *  threaded - true if your application is using threads, false otherwise.
   --

   function Mosquitto_Threaded_Set (Mosq : System.Address; Threaded : Extensions.Bool) return Int;  -- /usr/include/mosquitto.h:928
   pragma Import (C, Mosquitto_Threaded_Set, "mosquitto_threaded_set");

   -- * Function: mosquitto_opts_set
   -- *
   -- * Used to set options for the client.
   -- *
   -- * Parameters:
   -- *	mosq -   a valid mosquitto instance.
   -- *	option - the option to set.
   -- *	value -  the option specific value.
   -- *
   -- * Options:
   -- *	MOSQ_OPT_PROTOCOL_VERSION - value must be an int, set to either
   -- *	                            MQTT_PROTOCOL_V31 or MQTT_PROTOCOL_V311. Must
   -- *	                            be set before the client connects. Defaults to
   -- *	                            MQTT_PROTOCOL_V31.
   --

   function Mosquitto_Opts_Set
     (Mosq   : System.Address;
      Option : Mosq_Opt_T;
      Value  : System.Address) return Int;  -- /usr/include/mosquitto.h:946
   pragma Import (C, Mosquitto_Opts_Set, "mosquitto_opts_set");

   -- * Function: mosquitto_tls_set
   -- *
   -- * Configure the client for certificate based SSL/TLS support. Must be called
   -- * before <mosquitto_connect>.
   -- *
   -- * Cannot be used in conjunction with <mosquitto_tls_psk_set>.
   -- *
   -- * Define the Certificate Authority certificates to be trusted (ie. the server
   -- * certificate must be signed with one of these certificates) using cafile.
   -- *
   -- * If the server you are connecting to requires clients to provide a
   -- * certificate, define certfile and keyfile with your client certificate and
   -- * private key. If your private key is encrypted, provide a password callback
   -- * function or you will have to enter the password at the command line.
   -- *
   -- * Parameters:
   -- *  mosq -        a valid mosquitto instance.
   -- *  cafile -      path to a file containing the PEM encoded trusted CA
   -- *                certificate files. Either cafile or capath must not be NULL.
   -- *  capath -      path to a directory containing the PEM encoded trusted CA
   -- *                certificate files. See mosquitto.conf for more details on
   -- *                configuring this directory. Either cafile or capath must not
   -- *                be NULL.
   -- *  certfile -    path to a file containing the PEM encoded certificate file
   -- *                for this client. If NULL, keyfile must also be NULL and no
   -- *                client certificate will be used.
   -- *  keyfile -     path to a file containing the PEM encoded private key for
   -- *                this client. If NULL, certfile must also be NULL and no
   -- *                client certificate will be used.
   -- *  pw_callback - if keyfile is encrypted, set pw_callback to allow your client
   -- *                to pass the correct password for decryption. If set to NULL,
   -- *                the password must be entered on the command line.
   -- *                Your callback must write the password into "buf", which is
   -- *                "size" bytes long. The return value must be the length of the
   -- *                password. "userdata" will be set to the calling mosquitto
   -- *                instance.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * See Also:
   -- *	<mosquitto_tls_opts_set>, <mosquitto_tls_psk_set>, <mosquitto_tls_insecure_set>
   --

   function Mosquitto_Tls_Set
     (Mosq        : System.Address;
      Cafile      : Interfaces.C.Strings.Chars_Ptr;
      Capath      : Interfaces.C.Strings.Chars_Ptr;
      Certfile    : Interfaces.C.Strings.Chars_Ptr;
      Keyfile     : Interfaces.C.Strings.Chars_Ptr;
      Pw_Callback : access function
        (Arg1 : Interfaces.C.Strings.Chars_Ptr;
         Arg2 : Int;
         Arg3 : Int;
         Arg4 : System.Address) return Int) return Int;  -- /usr/include/mosquitto.h:995
   pragma Import (C, Mosquitto_Tls_Set, "mosquitto_tls_set");

   -- * Function: mosquitto_tls_insecure_set
   -- *
   -- * Configure verification of the server hostname in the server certificate. If
   -- * value is set to true, it is impossible to guarantee that the host you are
   -- * connecting to is not impersonating your server. This can be useful in
   -- * initial server testing, but makes it possible for a malicious third party to
   -- * impersonate your server through DNS spoofing, for example.
   -- * Do not use this function in a real system. Setting value to true makes the
   -- * connection encryption pointless.
   -- * Must be called before <mosquitto_connect>.
   -- *
   -- * Parameters:
   -- *  mosq -  a valid mosquitto instance.
   -- *  value - if set to false, the default, certificate hostname checking is
   -- *          performed. If set to true, no hostname checking is performed and
   -- *          the connection is insecure.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- *
   -- * See Also:
   -- *	<mosquitto_tls_set>
   --

   function Mosquitto_Tls_Insecure_Set (Mosq : System.Address; Value : Extensions.Bool) return Int;  -- /usr/include/mosquitto.h:1025
   pragma Import (C, Mosquitto_Tls_Insecure_Set, "mosquitto_tls_insecure_set");

   -- * Function: mosquitto_tls_opts_set
   -- *
   -- * Set advanced SSL/TLS options. Must be called before <mosquitto_connect>.
   -- *
   -- * Parameters:
   -- *  mosq -        a valid mosquitto instance.
   -- *	cert_reqs -   an integer defining the verification requirements the client
   -- *	              will impose on the server. This can be one of:
   -- *	              * SSL_VERIFY_NONE (0): the server will not be verified in any way.
   -- *	              * SSL_VERIFY_PEER (1): the server certificate will be verified
   -- *	                and the connection aborted if the verification fails.
   -- *	              The default and recommended value is SSL_VERIFY_PEER. Using
   -- *	              SSL_VERIFY_NONE provides no security.
   -- *	tls_version - the version of the SSL/TLS protocol to use as a string. If NULL,
   -- *	              the default value is used. The default value and the
   -- *	              available values depend on the version of openssl that the
   -- *	              library was compiled against. For openssl >= 1.0.1, the
   -- *	              available options are tlsv1.2, tlsv1.1 and tlsv1, with tlv1.2
   -- *	              as the default. For openssl < 1.0.1, only tlsv1 is available.
   -- *	ciphers -     a string describing the ciphers available for use. See the
   -- *	              "openssl ciphers" tool for more information. If NULL, the
   -- *	              default ciphers will be used.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * See Also:
   -- *	<mosquitto_tls_set>
   --

   function Mosquitto_Tls_Opts_Set
     (Mosq        : System.Address;
      Cert_Reqs   : Int;
      Tls_Version : Interfaces.C.Strings.Chars_Ptr;
      Ciphers     : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:1059
   pragma Import (C, Mosquitto_Tls_Opts_Set, "mosquitto_tls_opts_set");

   -- * Function: mosquitto_tls_psk_set
   -- *
   -- * Configure the client for pre-shared-key based TLS support. Must be called
   -- * before <mosquitto_connect>.
   -- *
   -- * Cannot be used in conjunction with <mosquitto_tls_set>.
   -- *
   -- * Parameters:
   -- *  mosq -     a valid mosquitto instance.
   -- *  psk -      the pre-shared-key in hex format with no leading "0x".
   -- *  identity - the identity of this client. May be used as the username
   -- *             depending on the server settings.
   -- *	ciphers -  a string describing the PSK ciphers available for use. See the
   -- *	           "openssl ciphers" tool for more information. If NULL, the
   -- *	           default ciphers will be used.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * See Also:
   -- *	<mosquitto_tls_set>
   --

   function Mosquitto_Tls_Psk_Set
     (Mosq     : System.Address;
      Psk      : Interfaces.C.Strings.Chars_Ptr;
      Identity : Interfaces.C.Strings.Chars_Ptr;
      Ciphers  : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:1086
   pragma Import (C, Mosquitto_Tls_Psk_Set, "mosquitto_tls_psk_set");

   --
   -- * Function: mosquitto_connect_callback_setBlåsrör
   -- *
   -- * Set the connect callback. This is called when the broker sends a CONNACK
   -- * message in response to a connection.
   -- *
   -- * Parameters:
   -- *  mosq -       a valid mosquitto instance.
   -- *  on_connect - a callback function in the following form:
   -- *               void callback(struct mosquitto *mosq, void *obj, int rc)
   -- *
   -- * Callback Parameters:
   -- *  mosq - the mosquitto instance making the callback.
   -- *  obj - the user data provided in <mosquitto_new>
   -- *  rc -  the return code of the connection response, one of:
   -- *
   -- * * 0 - success
   -- * * 1 - connection refused (unacceptable protocol version)
   -- * * 2 - connection refused (identifier rejected)
   -- * * 3 - connection refused (broker unavailable)
   -- * * 4-255 - reserved for future use
   --

   procedure Mosquitto_Connect_Callback_Set (Mosq : System.Address; On_Connect : access procedure
                                               (Arg1 : System.Address;
                                                Arg2 : System.Address;
                                                Arg3 : Int));  -- /usr/include/mosquitto.h:1110
   pragma Import (C, Mosquitto_Connect_Callback_Set, "mosquitto_connect_callback_set");

   -- * Function: mosquitto_disconnect_callback_set
   -- *
   -- * Set the disconnect callback. This is called when the broker has received the
   -- * DISCONNECT command and has disconnected the client.
   -- *
   -- * Parameters:
   -- *  mosq -          a valid mosquitto instance.
   -- *  on_disconnect - a callback function in the following form:
   -- *                  void callback(struct mosquitto *mosq, void *obj)
   -- *
   -- * Callback Parameters:
   -- *  mosq - the mosquitto instance making the callback.
   -- *  obj -  the user data provided in <mosquitto_new>
   -- *  rc -   integer value indicating the reason for the disconnect. A value of 0
   -- *         means the client has called <mosquitto_disconnect>. Any other value
   -- *         indicates that the disconnect is unexpected.
   --

   procedure Mosquitto_Disconnect_Callback_Set (Mosq : System.Address; On_Disconnect : access procedure
                                                  (Arg1 : System.Address;
                                                   Arg2 : System.Address;
                                                   Arg3 : Int));  -- /usr/include/mosquitto.h:1130
   pragma Import (C, Mosquitto_Disconnect_Callback_Set, "mosquitto_disconnect_callback_set");

   -- * Function: mosquitto_publish_callback_set
   -- *
   -- * Set the publish callback. This is called when a message initiated with
   -- * <mosquitto_publish> has been sent to the broker successfully.
   -- *
   -- * Parameters:
   -- *  mosq -       a valid mosquitto instance.
   -- *  on_publish - a callback function in the following form:
   -- *               void callback(struct mosquitto *mosq, void *obj, int mid)
   -- *
   -- * Callback Parameters:
   -- *  mosq - the mosquitto instance making the callback.
   -- *  obj -  the user data provided in <mosquitto_new>
   -- *  mid -  the message id of the sent message.
   --

   procedure Mosquitto_Publish_Callback_Set (Mosq : System.Address; On_Publish : access procedure
                                               (Arg1 : System.Address;
                                                Arg2 : System.Address;
                                                Arg3 : Int));  -- /usr/include/mosquitto.h:1148
   pragma Import (C, Mosquitto_Publish_Callback_Set, "mosquitto_publish_callback_set");

   -- * Function: mosquitto_message_callback_set
   -- *
   -- * Set the message callback. This is called when a message is received from the
   -- * broker.
   -- *
   -- * Parameters:
   -- *  mosq -       a valid mosquitto instance.
   -- *  on_message - a callback function in the following form:
   -- *               void callback(struct mosquitto *mosq, void *obj, const struct mosquitto_message *message)
   -- *
   -- * Callback Parameters:
   -- *  mosq -    the mosquitto instance making the callback.
   -- *  obj -     the user data provided in <mosquitto_new>
   -- *  message - the message data. This variable and associated memory will be
   -- *            freed by the library after the callback completes. The client
   -- *            should make copies of any of the data it requires.
   -- *
   -- * See Also:
   -- * 	<mosquitto_message_copy>
   --

   procedure Mosquitto_Message_Callback_Set (Mosq : System.Address; On_Message : access procedure
                                               (Arg1 : System.Address;
                                                Arg2 : System.Address;
                                                Arg3 : access constant Mosquitto_Message));  -- /usr/include/mosquitto.h:1171
   pragma Import (C, Mosquitto_Message_Callback_Set, "mosquitto_message_callback_set");

   -- * Function: mosquitto_subscribe_callback_set
   -- *
   -- * Set the subscribe callback. This is called when the broker responds to a
   -- * subscription request.
   -- *
   -- * Parameters:
   -- *  mosq -         a valid mosquitto instance.
   -- *  on_subscribe - a callback function in the following form:
   -- *                 void callback(struct mosquitto *mosq, void *obj, int mid, int qos_count, const int *granted_qos)
   -- *
   -- * Callback Parameters:
   -- *  mosq -        the mosquitto instance making the callback.
   -- *  obj -         the user data provided in <mosquitto_new>
   -- *  mid -         the message id of the subscribe message.
   -- *  qos_count -   the number of granted subscriptions (size of granted_qos).
   -- *  granted_qos - an array of integers indicating the granted QoS for each of
   -- *                the subscriptions.
   --

   procedure Mosquitto_Subscribe_Callback_Set (Mosq : System.Address; On_Subscribe : access procedure
                                                 (Arg1 : System.Address;
                                                  Arg2 : System.Address;
                                                  Arg3 : Int;
                                                  Arg4 : Int;
                                                  Arg5 : access Int));  -- /usr/include/mosquitto.h:1192
   pragma Import (C, Mosquitto_Subscribe_Callback_Set, "mosquitto_subscribe_callback_set");

   -- * Function: mosquitto_unsubscribe_callback_set
   -- *
   -- * Set the unsubscribe callback. This is called when the broker responds to a
   -- * unsubscription request.
   -- *
   -- * Parameters:
   -- *  mosq -           a valid mosquitto instance.
   -- *  on_unsubscribe - a callback function in the following form:
   -- *                   void callback(struct mosquitto *mosq, void *obj, int mid)
   -- *
   -- * Callback Parameters:
   -- *  mosq - the mosquitto instance making the callback.
   -- *  obj -  the user data provided in <mosquitto_new>
   -- *  mid -  the message id of the unsubscribe message.
   --

   procedure Mosquitto_Unsubscribe_Callback_Set (Mosq : System.Address; On_Unsubscribe : access procedure
                                                   (Arg1 : System.Address;
                                                    Arg2 : System.Address;
                                                    Arg3 : Int));  -- /usr/include/mosquitto.h:1210
   pragma Import (C, Mosquitto_Unsubscribe_Callback_Set, "mosquitto_unsubscribe_callback_set");

   -- * Function: mosquitto_log_callback_set
   -- *
   -- * Set the logging callback. This should be used if you want event logging
   -- * information from the client library.
   -- *
   -- *  mosq -   a valid mosquitto instance.
   -- *  on_log - a callback function in the following form:
   -- *           void callback(struct mosquitto *mosq, void *obj, int level, const char *str)
   -- *
   -- * Callback Parameters:
   -- *  mosq -  the mosquitto instance making the callback.
   -- *  obj -   the user data provided in <mosquitto_new>
   -- *  level - the log message level from the values:
   -- *	        MOSQ_LOG_INFO
   -- *	        MOSQ_LOG_NOTICE
   -- *	        MOSQ_LOG_WARNING
   -- *	        MOSQ_LOG_ERR
   -- *	        MOSQ_LOG_DEBUG
   -- *	str -   the message string.
   --

   procedure Mosquitto_Log_Callback_Set (Mosq : System.Address; On_Log : access procedure
                                           (Arg1 : System.Address;
                                            Arg2 : System.Address;
                                            Arg3 : Int;
                                            Arg4 : Interfaces.C.Strings.Chars_Ptr));  -- /usr/include/mosquitto.h:1233
   pragma Import (C, Mosquitto_Log_Callback_Set, "mosquitto_log_callback_set");

   -- * Function: mosquitto_reconnect_delay_set
   -- *
   -- * Control the behaviour of the client when it has unexpectedly disconnected in
   -- * <mosquitto_loop_forever> or after <mosquitto_loop_start>. The default
   -- * behaviour if this function is not used is to repeatedly attempt to reconnect
   -- * with a delay of 1 second until the connection succeeds.
   -- *
   -- * Use reconnect_delay parameter to change the delay between successive
   -- * reconnection attempts. You may also enable exponential backoff of the time
   -- * between reconnections by setting reconnect_exponential_backoff to true and
   -- * set an upper bound on the delay with reconnect_delay_max.
   -- *
   -- * Example 1:
   -- *	delay=2, delay_max=10, exponential_backoff=False
   -- *	Delays would be: 2, 4, 6, 8, 10, 10, ...
   -- *
   -- * Example 2:
   -- *	delay=3, delay_max=30, exponential_backoff=True
   -- *	Delays would be: 3, 6, 12, 24, 30, 30, ...
   -- *
   -- * Parameters:
   -- *  mosq -                          a valid mosquitto instance.
   -- *  reconnect_delay -               the number of seconds to wait between
   -- *                                  reconnects.
   -- *  reconnect_delay_max -           the maximum number of seconds to wait
   -- *                                  between reconnects.
   -- *  reconnect_exponential_backoff - use exponential backoff between
   -- *                                  reconnect attempts. Set to true to enable
   -- *                                  exponential backoff.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   --

   function Mosquitto_Reconnect_Delay_Set
     (Mosq                          : System.Address;
      Reconnect_Delay               : Unsigned;
      Reconnect_Delay_Max           : Unsigned;
      Reconnect_Exponential_Backoff : Extensions.Bool) return Int;  -- /usr/include/mosquitto.h:1270
   pragma Import (C, Mosquitto_Reconnect_Delay_Set, "mosquitto_reconnect_delay_set");

   -- * Function: mosquitto_max_inflight_messages_set
   -- *
   -- * Set the number of QoS 1 and 2 messages that can be "in flight" at one time.
   -- * An in flight message is part way through its delivery flow. Attempts to send
   -- * further messages with <mosquitto_publish> will result in the messages being
   -- * queued until the number of in flight messages reduces.
   -- *
   -- * A higher number here results in greater message throughput, but if set
   -- * higher than the maximum in flight messages on the broker may lead to
   -- * delays in the messages being acknowledged.
   -- *
   -- * Set to 0 for no maximum.
   -- *
   -- * Parameters:
   -- *  mosq -                  a valid mosquitto instance.
   -- *  max_inflight_messages - the maximum number of inflight messages. Defaults
   -- *                          to 20.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success.
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   --

   function Mosquitto_Max_Inflight_Messages_Set (Mosq : System.Address; Max_Inflight_Messages : Unsigned) return Int;  -- /usr/include/mosquitto.h:1295
   pragma Import (C, Mosquitto_Max_Inflight_Messages_Set, "mosquitto_max_inflight_messages_set");

   -- * Function: mosquitto_message_retry_set
   -- *
   -- * Set the number of seconds to wait before retrying messages. This applies to
   -- * publish messages with QoS>0. May be called at any time.
   -- *
   -- * Parameters:
   -- *  mosq -          a valid mosquitto instance.
   -- *  message_retry - the number of seconds to wait for a response before
   -- *                  retrying. Defaults to 20.
   --

   procedure Mosquitto_Message_Retry_Set (Mosq : System.Address; Message_Retry : Unsigned);  -- /usr/include/mosquitto.h:1308
   pragma Import (C, Mosquitto_Message_Retry_Set, "mosquitto_message_retry_set");

   -- * Function: mosquitto_user_data_set
   -- *
   -- * When <mosquitto_new> is called, the pointer given as the "obj" parameter
   -- * will be passed to the callbacks as user data. The <mosquitto_user_data_set>
   -- * function allows this obj parameter to be updated at any time. This function
   -- * will not modify the memory pointed to by the current user data pointer. If
   -- * it is dynamically allocated memory you must free it yourself.
   -- *
   -- * Parameters:
   -- *  mosq - a valid mosquitto instance.
   -- * 	obj -  A user pointer that will be passed as an argument to any callbacks
   -- * 	       that are specified.
   --

   procedure Mosquitto_User_Data_Set (Mosq : System.Address; Obj : System.Address);  -- /usr/include/mosquitto.h:1324
   pragma Import (C, Mosquitto_User_Data_Set, "mosquitto_user_data_set");

   -- =============================================================================
   -- *
   -- * Section: SOCKS5 proxy functions
   -- *
   -- * =============================================================================
   --

   -- * Function: mosquitto_socks5_set
   -- *
   -- * Configure the client to use a SOCKS5 proxy when connecting. Must be called
   -- * before connecting. "None" and "username/password" authentication is
   -- * supported.
   -- *
   -- * Parameters:
   -- *   mosq - a valid mosquitto instance.
   -- *   host - the SOCKS5 proxy host to connect to.
   -- *   port - the SOCKS5 proxy port to use.
   -- *   username - if not NULL, use this username when authenticating with the proxy.
   -- *   password - if not NULL and username is not NULL, use this password when
   -- *              authenticating with the proxy.
   --

   function Mosquitto_Socks5_Set
     (Mosq     : System.Address;
      Host     : Interfaces.C.Strings.Chars_Ptr;
      Port     : Int;
      Username : Interfaces.C.Strings.Chars_Ptr;
      Password : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:1348
   pragma Import (C, Mosquitto_Socks5_Set, "mosquitto_socks5_set");

   -- =============================================================================
   -- *
   -- * Section: Utility functions
   -- *
   -- * =============================================================================
   --

   -- * Function: mosquitto_strerror
   -- *
   -- * Call to obtain a const string description of a mosquitto error number.
   -- *
   -- * Parameters:
   -- *	mosq_errno - a mosquitto error number.
   -- *
   -- * Returns:
   -- *	A constant string describing the error.
   --

   function Mosquitto_Strerror (Mosq_Errno : Int) return Interfaces.C.Strings.Chars_Ptr;  -- /usr/include/mosquitto.h:1368
   pragma Import (C, Mosquitto_Strerror, "mosquitto_strerror");

   -- * Function: mosquitto_connack_string
   -- *
   -- * Call to obtain a const string description of an MQTT connection result.
   -- *
   -- * Parameters:
   -- *	connack_code - an MQTT connection result.
   -- *
   -- * Returns:
   -- *	A constant string describing the result.
   --

   function Mosquitto_Connack_String (Connack_Code : Int) return Interfaces.C.Strings.Chars_Ptr;  -- /usr/include/mosquitto.h:1381
   pragma Import (C, Mosquitto_Connack_String, "mosquitto_connack_string");

   -- * Function: mosquitto_sub_topic_tokenise
   -- *
   -- * Tokenise a topic or subscription string into an array of strings
   -- * representing the topic hierarchy.
   -- *
   -- * For example:
   -- *
   -- * subtopic: "a/deep/topic/hierarchy"
   -- *
   -- * Would result in:
   -- *
   -- * topics[0] = "a"
   -- * topics[1] = "deep"
   -- * topics[2] = "topic"
   -- * topics[3] = "hierarchy"
   -- *
   -- * and:
   -- *
   -- * subtopic: "/a/deep/topic/hierarchy/"
   -- *
   -- * Would result in:
   -- *
   -- * topics[0] = NULL
   -- * topics[1] = "a"
   -- * topics[2] = "deep"
   -- * topics[3] = "topic"
   -- * topics[4] = "hierarchy"
   -- *
   -- * Parameters:
   -- *	subtopic - the subscription/topic to tokenise
   -- *	topics -   a pointer to store the array of strings
   -- *	count -    an int pointer to store the number of items in the topics array.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   -- *
   -- * Example:
   -- *
   -- * > char **topics;
   -- * > int topic_count;
   -- * > int i;
   -- * >
   -- * > mosquitto_sub_topic_tokenise("$SYS/broker/uptime", &topics, &topic_count);
   -- * >
   -- * > for(i=0; i<token_count; i++){
   -- * >     printf("%d: %s\n", i, topics[i]);
   -- * > }
   -- *
   -- * See Also:
   -- *	<mosquitto_sub_topic_tokens_free>
   --

   function Mosquitto_Sub_Topic_Tokenise
     (Subtopic : Interfaces.C.Strings.Chars_Ptr;
      Topics   : System.Address;
      Count    : access Int) return Int;  -- /usr/include/mosquitto.h:1436
   pragma Import (C, Mosquitto_Sub_Topic_Tokenise, "mosquitto_sub_topic_tokenise");

   -- * Function: mosquitto_sub_topic_tokens_free
   -- *
   -- * Free memory that was allocated in <mosquitto_sub_topic_tokenise>.
   -- *
   -- * Parameters:
   -- *	topics - pointer to string array.
   -- *	count - count of items in string array.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- *
   -- * See Also:
   -- *	<mosquitto_sub_topic_tokenise>
   --

   function Mosquitto_Sub_Topic_Tokens_Free (Topics : System.Address; Count : Int) return Int;  -- /usr/include/mosquitto.h:1454
   pragma Import (C, Mosquitto_Sub_Topic_Tokens_Free, "mosquitto_sub_topic_tokens_free");

   -- * Function: mosquitto_topic_matches_sub
   -- *
   -- * Check whether a topic matches a subscription.
   -- *
   -- * For example:
   -- *
   -- * foo/bar would match the subscription foo/# or +/bar
   -- * non/matching would not match the subscription non/+/+
   -- *
   -- * Parameters:
   -- *	sub - subscription string to check topic against.
   -- *	topic - topic to check.
   -- *	result - bool pointer to hold result. Will be set to true if the topic
   -- *	         matches the subscription.
   -- *
   -- * Returns:
   -- *	MOSQ_ERR_SUCCESS - on success
   -- * 	MOSQ_ERR_INVAL -   if the input parameters were invalid.
   -- * 	MOSQ_ERR_NOMEM -   if an out of memory condition occurred.
   --

   function Mosquitto_Topic_Matches_Sub
     (Sub    : Interfaces.C.Strings.Chars_Ptr;
      Topic  : Interfaces.C.Strings.Chars_Ptr;
      Result : access Extensions.Bool) return Int;  -- /usr/include/mosquitto.h:1477
   pragma Import (C, Mosquitto_Topic_Matches_Sub, "mosquitto_topic_matches_sub");

   -- * Function: mosquitto_pub_topic_check
   -- *
   -- * Check whether a topic to be used for publishing is valid.
   -- *
   -- * This searches for + or # in a topic and checks its length.
   -- *
   -- * This check is already carried out in <mosquitto_publish> and
   -- * <mosquitto_will_set>, there is no need to call it directly before them. It
   -- * may be useful if you wish to check the validity of a topic in advance of
   -- * making a connection for example.
   -- *
   -- * Parameters:
   -- *   topic - the topic to check
   -- *
   -- * Returns:
   -- *   MOSQ_ERR_SUCCESS - for a valid topic
   -- *   MOSQ_ERR_INVAL - if the topic contains a + or a #, or if it is too long.
   -- *
   -- * See Also:
   -- *   <mosquitto_sub_topic_check>
   --

   function Mosquitto_Pub_Topic_Check (Topic : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:1501
   pragma Import (C, Mosquitto_Pub_Topic_Check, "mosquitto_pub_topic_check");

   -- * Function: mosquitto_sub_topic_check
   -- *
   -- * Check whether a topic to be used for subscribing is valid.
   -- *
   -- * This searches for + or # in a topic and checks that they aren't in invalid
   -- * positions, such as with foo/#/bar, foo/+bar or foo/bar#, and checks its
   -- * length.
   -- *
   -- * This check is already carried out in <mosquitto_subscribe> and
   -- * <mosquitto_unsubscribe>, there is no need to call it directly before them.
   -- * It may be useful if you wish to check the validity of a topic in advance of
   -- * making a connection for example.
   -- *
   -- * Parameters:
   -- *   topic - the topic to check
   -- *
   -- * Returns:
   -- *   MOSQ_ERR_SUCCESS - for a valid topic
   -- *   MOSQ_ERR_INVAL - if the topic contains a + or a # that is in an invalid
   -- *                    position, or if it is too long.
   -- *
   -- * See Also:
   -- *   <mosquitto_sub_topic_check>
   --

   function Mosquitto_Sub_Topic_Check (Topic : Interfaces.C.Strings.Chars_Ptr) return Int;  -- /usr/include/mosquitto.h:1528
   pragma Import (C, Mosquitto_Sub_Topic_Check, "mosquitto_sub_topic_check");

end Mosquitto.Mosquitto_H;
