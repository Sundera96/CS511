-module(chatroom).

-include_lib("./defs.hrl").

-export([start_chatroom/1]).

-spec start_chatroom(_ChatName) -> _.
-spec loop(_State) -> _.
-spec do_register(_State, _Ref, _ClientPID, _ClientNick) -> _NewState.
-spec do_unregister(_State, _ClientPID) -> _NewState.
-spec do_update_nick(_State, _ClientPID, _NewNick) -> _NewState.
-spec do_propegate_message(_State, _Ref, _ClientPID, _Message) -> _NewState.

start_chatroom(ChatName) ->
    loop(#chat_st{name = ChatName,
		  registrations = maps:new(), history = []}),
    ok.

loop(State) ->
    NewState =
	receive
	    %% Server tells this chatroom to register a client
	    {_ServerPID, Ref, register, ClientPID, ClientNick} ->
		do_register(State, Ref, ClientPID, ClientNick);
	    %% Server tells this chatroom to unregister a client
	    {_ServerPID, _Ref, unregister, ClientPID} ->
		do_unregister(State, ClientPID);
	    %% Server tells this chatroom to update the nickname for a certain client
	    {_ServerPID, _Ref, update_nick, ClientPID, NewNick} ->
		do_update_nick(State, ClientPID, NewNick);
	    %% Client sends a new message to the chatroom, and the chatroom must
	    %% propegate to other registered clients
	    {ClientPID, Ref, message, Message} ->
		do_propegate_message(State, Ref, ClientPID, Message);
	    {TEST_PID, get_state} ->
		TEST_PID!{get_state, State},
		loop(State)
end,
    loop(NewState).

%% This function should register a new client to this chatroom
do_register(State, Ref, ClientPID, ClientNick) ->
    io:format("chatroom:do_register(...): IMPLEMENT ME~n"),
	Client_Registration = maps:put(ClientPID,ClientNick,State#chat_st.registrations),
	io:format("Successfully refreshed client list in chatroom ~n"),
	ClientPID ! {self(), Ref, connect, State#chat_st.history},
	#chat_st{
		name = State#chat_st.name,
		registrations = Client_Registration,
		history = State#chat_st.history
	}.

%% This function should unregister a client from this chatroom
do_unregister(State, ClientPID) ->
    io:format("chatroom:do_unregister(...): IMPLEMENT ME~n"),
	Updated_Client_State = maps:remove(ClientPID,State#chat_st.registrations),
	whereis(server) ! {removed_client_from_Chat},
    #chat_st{
		name = State#chat_st.name,
		registrations = Updated_Client_State,
		history = State#chat_st.history
	}.

%% This function should update the nickname of specified client.
do_update_nick(State, ClientPID, NewNick) ->
    io:format("chatroom:do_update_nick(...): IMPLEMENT ME~n"),
	Updated_Nick_State= maps:put(ClientPID,NewNick,State#chat_st.registrations),
	whereis(server) ! {nick_is_updated},
    #chat_st{
		name = State#chat_st.name,
		registrations = Updated_Nick_State,
		history = State#chat_st.history
	}.

%% This function should update all clients in chatroom with new message
%% (read assignment specs for details)
do_propegate_message(State, Ref, ClientPID, Message) ->
	Receiever_PID = maps:remove(ClientPID,State#chat_st.registrations),
	{ok,Client_Nick_Name} = maps:find(ClientPID,State#chat_st.registrations),
	maps:fold(fun(K,V,_)->
		io:format("Sending to nick name ~s~n",[V]),
		K ! {request, self(), Ref, {incoming_msg,Client_Nick_Name,State#chat_st.name, Message}}
	 end,ok,Receiever_PID),
    io:format("chatroom:do_propegate_message(...): IMPLEMENT ME~n"),
	ClientPID ! {self(),Ref,ack_msg},
    #chat_st{
		name = State#chat_st.name,
		registrations = State#chat_st.registrations,
		history = lists:append(State#chat_st.history,[{Client_Nick_Name,Message}])
	}.
