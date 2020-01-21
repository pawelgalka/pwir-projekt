-module(data_manager).

%% API
-compile(export_all).

init() -> ets:new(states, [ordered_set, public, named_table]).
lookup(Container, Key) -> whereis(Key).

lookup_state(Container, Key) ->
  element(2, hd(ets:lookup(Container, Key))).

create_process(Container, Name, Key) ->
  if is_pid(Key) -> register(Name, Key);
    true -> ets:insert(states, {Name, Key})
  end.
%%  ets:insert(Container, {Name, Key}).

delete_process(Container, Key) ->
  PID = lookup(Container, Key),
  io:format("Data Manager is killing process ~p ~p~n", [Key, PID]),
  exit(PID, stop).

close(Container) ->
  ets:delete(Container).


