-module(channel).
-export([start/1,stop/1]).

-record(channel_st, {
  channel, % Channel atom
  clients % Client PIDs
}).


initial_state(ChannelAtom) ->
  #channel_st{
    channel = ChannelAtom,
    clients = []
  }.
% Starting and Stopping the Channel
start(ChannelAtom) ->
  genserver:start(list_to_atom(ChannelAtom), initial_state(ChannelAtom), fun handle/2).
stop(ChannelAtom) ->
  genserver:stop(ChannelAtom).





% Client Channel join
handle(St, {join, Client}) ->
  case lists:member(Client, St#channel_st.clients) of
    true  -> {reply, user_already_joined, St};
    false -> {reply, ok, St#channel_st{clients = [Client| St#channel_st.clients]}}
  end;

% Client Channel leave
handle(St, {leave, Client}) ->
  case lists:member(Client, St#channel_st.clients) of
    true -> {reply, ok, St#channel_st{clients = lists:delete(Client, St#channel_st.clients)}};

    false -> {reply, user_not_joined, St}
  end;

% Client Message Sending
handle(St, {message_send, Client, Nick, Message}) ->
  case lists:member(Client, St#channel_st.clients) of

    true ->
      [spawn(
         fun () ->
           genserver:request(Receiver, {message_receive, St#channel_st.channel, Nick, Message}) end) ||
             Receiver <- lists:delete(Client, St#channel_st.clients)],
      {reply, ok, St};

    false ->
      {reply, user_not_joined, St}
  end;

% Terminate the Channel
%handle(_, {termination, Channel}) ->
 % stop(Channel),
 % {reply, ok, []};

% Catch-all for any unhandled requests
handle(St, _Data) ->
  {reply, {error, not_implemented, "Channel does not handle this command"}, St} .