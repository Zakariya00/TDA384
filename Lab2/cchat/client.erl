-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.





% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client


% Helper functions
isAlive(St) -> lists:member(St, registered()).
requesting(To, Content) ->
    try genserver:request(To, Content) of
        Received -> Received
    catch
        error:_ -> {error, server_not_reached, "No response form Server!!!"};
        exit:_ -> {error, server_not_reached, "No response form Server!!!"};
        throw:_ -> {error, server_not_reached, "No response form Server!!!"}
    end .



% Join channel
handle(St, {join, Channel}) ->
    case isAlive(St#client_st.server) of
        true ->
            case requesting(St#client_st.server, {join, self(), Channel, St#client_st.nick}) of
                ok ->
                    Channels = [Channel| St#client_st.channels],
                    {reply, ok, St#client_st{channels = Channels}};

                user_already_joined ->
                    {reply, {error, user_already_joined, "Already joined the channel!"}, St};

                Failure ->
                    {reply, Failure, St}
            end;

        false ->
            {reply, {error, server_not_reached, "Server doesnt exist!!!"}, St}
    end;


% Leave channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of

        true ->
            requesting(list_to_atom(Channel), {leave, self()}),
            {reply, ok, St#client_st{channels = lists:delete(Channel, St#client_st.channels)}};

        false ->
            {reply, {error, user_not_joined, "Cant leave the channel cause youre not a member fool!!! "}, St}
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case requesting(list_to_atom(Channel), {message_send, self(), St#client_st.nick, Msg}) of

        ok -> {reply, ok, St};

        user_not_joined -> {reply, {error, user_not_joined, "Not in the channel stupid!!"}, St};

        Error -> {reply, Error, St}
    end;


% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    case requesting(St#client_st.server, {nick, St#client_st.nick, NewNick}) of
        ok -> {reply, ok, St#client_st{nick = NewNick}};

        nick_taken -> {reply, {error, nick_taken, "This Nick is taken!!"}, St};

        Error -> {reply, Error, St}
    end;






% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    [requesting(list_to_atom(Channel), {leave, self()}) || Channel <- St#client_st.channels],  %tell your channels
    requesting(St#client_st.server, {leave, St#client_st.nick}), % tell server
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
