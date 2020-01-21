-module(process_orchestrator).

%% API
-compile(export_all).

pid() -> self().

states_set() -> states.

process_listener() -> process_listener.

gui_PID() ->
  data_manager:lookup(guiPID).

init() ->
  io:format("Process manager is initialized ~p~n", [pid()]),
  data_manager:init(),
  initialized.

invoke_listener() ->
  register(process_listener(), spawn(?MODULE, listen, [])),
  whereis(process_listener()).

stop() ->
  io:format("Process manager is stopped ~p~n", [self()]),
  data_manager:delete_process(process_listener()).

close_app() ->
  data_manager:save_values(),
  ets:delete(states),
  data_manager:delete_process(guiPID),
  close.

listen() ->
  receive
    {create, Key, Pid} -> io:format("Creating process in container ~s at id ~s~p~n", [states_set(), Key,Pid]),
      data_manager:create_process(Key, Pid),
      listen();
    {delete, Key} -> io:format("Deleting process from container ~s at id ~s~n", [states_set(), Key]),
      data_manager:delete_process(Key),
      io:format("Deleted process from container ~s at id ~s~n", [states_set(), Key]),
      listen();
    {stop} -> io:format("Ending storing in ~s", [states_set()]), data_manager:close(states_set()),
      listen()
  end.

close() ->
  io:format("Stopping listener ~n"), exit(data_manager:lookup(process_listener()), stop),
  io:format("Stopping orchestrator ~n"), exit(pid(), stop).


