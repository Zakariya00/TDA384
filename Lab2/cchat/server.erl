-module(server).
-export([start/1,stop/1]).

% This record defines the structure of the state of a server.
% Add whatever other fields you need.
-record(server_st, {
    nicks, % nick/usernames of the clients
    channels % server channels
}).

% Return an initial state record
initial_state() ->
    #server_st{
        nicks = [],
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    catch (genserver:start(ServerAtom, initial_state(), fun handle/2)).
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, {termination}), % send termination message to server to kill channels
    genserver:stop(ServerAtom).




% Helper Functions
updateNicks(St, Nick) ->
    case lists:member(Nick, St#server_st.nicks) of
        true -> St#server_st.nicks;
        false -> [Nick|St#server_st.nicks]
    end.

updateChannel(St, Channel) ->
    case lists:member(Channel, St#server_st.channels) of
        true -> St#server_st.channels;
        false -> channel:start(Channel),     % start a new channel
            [Channel| St#server_st.channels]
    end.




% Client Joining A Channel
handle(St, {join, From, Channel, Nick}) ->
    UpdatedChannels = updateChannel(St, Channel),
    UpdatedNicks    = updateNicks(St, Nick),

    ChannelCommunication = genserver:request(list_to_atom(Channel), {join, From}),
    {reply, ChannelCommunication, St#server_st{nicks = UpdatedNicks,
                                               channels = UpdatedChannels}};

% Client Leaving
handle(St, {leave, Nick}) ->
    {reply, ok, St#server_st{nicks = lists:delete(Nick, St#server_st.nicks)}};

% Client Nick Change
handle(St, {nick, Old, New}) ->
    case lists:member(New, St#server_st.nicks) of
        true -> {reply, nick_taken, St};
        false -> {reply, ok, St#server_st{nicks = [New| lists:delete(Old, St#server_st.nicks)]}}
    end;

% Stop all Channels With The Server
handle(St, {termination}) ->
    [genserver:request(Channel, {terminate, Channel}) || Channel <- St#server_st.channels], % Tell channels to terminate
    {reply, ok, []};

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Server does not handle this command"}, St} .


