-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    io:format("server:do_join(...): IMPLEMENT ME~n"),
    ChatRoomPID = case maps:find(ChatName,State#serv_st.chatrooms) of
		{ok,Value}->
			Value;
		error->
			spawn(chatroom,start_chatroom,[ChatName])
	end,

	Client_Nickname = case maps:find(ClientPID,State#serv_st.nicks) of
		{ok,Nickname}->
			Nickname;
		error ->
			client_not_have_nicks
		end,
	
	ChatRoomPID!{self(), Ref, register, ClientPID, Client_Nickname},
	
	Chatroom_PID =  maps:find(ChatName,State#serv_st.chatrooms),
	Chatroom_PID_Store = case Chatroom_PID of
		{ok,_}->
			State#serv_st.chatrooms;
		error->
			maps:put(ChatName,ChatRoomPID,State#serv_st.chatrooms)
		end,
	
	io:format("Ping from chatroom~n"),
	Chatroom_Client_Map_Elem = maps:find(ChatName,State#serv_st.registrations),
	Chatroom_Client_Map = case Chatroom_Client_Map_Elem of 
		{ok,List_Of_Client}->
			List_Of_Client_New = lists:append(List_Of_Client,[ClientPID]),
			maps:put(ChatName,List_Of_Client_New,State#serv_st.registrations);
		error->
			io:format("Adding client pid to chat server~n"),
			maps:put(ChatName,[ClientPID],State#serv_st.registrations)
		end,
	#serv_st{
		nicks = State#serv_st.nicks,
		registrations = Chatroom_Client_Map,
		chatrooms =  Chatroom_PID_Store
		}.
	

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    io:format("server:do_leave(...): IMPLEMENT ME~n"),
	Chatroom_PID_Tuple = maps:find(ChatName,State#serv_st.chatrooms),
	Chatroom_PID = case Chatroom_PID_Tuple of
		{ok, Chatroom_PID_Value}->
			Chatroom_PID_Value;
		error->
			error
		end,
	
	%Removing client from chatroom registration
	Client_To_Chat_List_Tuple = maps:find(ChatName,State#serv_st.registrations),
	Updated_Client_To_Chat_List_State = case Client_To_Chat_List_Tuple of
		{ok,Client_To_Chat_List}->
			Updated_Client_To_Chat_List =  lists:delete(Chatroom_PID,Client_To_Chat_List),
			maps:put(ChatName,Updated_Client_To_Chat_List,State#serv_st.registrations)
		end,
	Chatroom_PID ! {self(), Ref, unregister, ClientPID},
	receive
		{removed_client_from_Chat}->
			ClientPID ! {self(), Ref, ack_leave}
		end,
    #serv_st{
		nicks = State#serv_st.nicks,
		registrations = Updated_Client_To_Chat_List_State,
		chatrooms =  State#serv_st.chatrooms
		}.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    io:format("server:do_new_nick(...): IMPLEMENT ME~n"),
    State.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
    State.
