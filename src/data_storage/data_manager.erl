-module(data_manager).

%% API
-compile(export_all).

init() -> ets:new(states, [ordered_set, public, named_table]).

lookup(Key) -> whereis(Key).

lookup_state(Key) ->
  element(2, hd(ets:lookup(states, Key))).

create_process(Name, Key) ->
  if is_pid(Key) -> register(Name, Key);
    true -> ets:insert(states, {Name, Key})
  end.

delete_process(Key) ->
  PID = lookup(Key),
  io:format("Data Manager is killing process ~p ~p~n", [Key, PID]),
  exit(PID, stop).

close(Container) ->
  ets:delete(Container).


