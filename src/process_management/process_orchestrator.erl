-module(process_orchestrator).

%% API
-compile(export_all).

pid() -> self().

processes_set() -> process_ids.

process_listener() -> process_listener.

gui_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), guiPID).

init() ->
  ets:new(processes_set(), [ordered_set, public, named_table]),
  io:format("Process manager is initialized ~p~n", [pid()]),
  initialized.

invoke_listener() ->
  ets:insert(processes_set(), {process_listener(), spawn(?MODULE, listen, [])}),
  data_manager:lookup(processes_set(), process_listener()).

stop() ->
  io:format("Process manager is stopped ~p~n", [pid()]),
  stop.

listen() ->
  receive
    {create, Key, Pid} -> io:format("Creating process in container ~s at id ~s~n", [processes_set(), Key]),
      data_manager:create_process(processes_set(), Key, Pid),
      listen();
    {delete, Key} -> io:format("Deleting process from container ~s at id ~s~n", [processes_set(), Key]),
      data_manager:delete_process(processes_set(), Key),
      io:format("Deleted process from container ~s at id ~s~n", [processes_set(), Key]),
      listen();
    {stop} -> io:format("Ending storing in ~s", [processes_set()]), data_manager:close(processes_set()),
      listen()
  end.

close() ->
  io:format("Stopping listener ~n"), exit(data_manager:lookup(processes_set(), process_listener()), stop),
  io:format("Stopping orchestrator ~n"), exit(pid(), stop).


