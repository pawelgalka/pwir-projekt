-module(data_manager).

%% API
-compile(export_all).

lookup(Container, Key) -> element(2, hd(ets:lookup(Container, Key))).

create_process(Container, Name, Key) -> ets:insert(Container, {Name, Key}).

delete_process(Container, Key) ->
  PID = lookup(Container, Key),
  io:format("Data Manager is killing process ~p ~p~n", [Key, PID]),
  exit(PID, stop).

close(Container) ->
  ets:delete(Container).


