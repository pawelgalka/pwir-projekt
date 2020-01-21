-module(data_manager).

%% API
-compile(export_all).

init() -> ets:new(states, [ordered_set, public, named_table]).
lookup(Key) -> whereis(Key).

minTemp() -> lookup_state(minTemp).
maxTemp() -> lookup_state(maxTemp).

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

save_values() ->
  {ok, Values} = file:open("init_states.txt", [write]),
  io:format(Values, "~p ~n~p", [minTemp(), maxTemp()]),
  file:close(Values).