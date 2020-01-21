-module(process_orchestrator).

%% API
-compile(export_all).

pid() -> self().

%% TODO: migrate processes from ets to erlang
processes_set() -> states.

process_listener() -> process_listener.

gui_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), guiPID).

init() ->
  io:format("Process manager is initialized ~p~n", [pid()]),
  data_manager:init(),
  initialized.

invoke_listener() ->
  register(process_listener(), spawn(?MODULE, listen, [])),
  whereis(process_listener()).
%%  ets:insert(processes_set(), {process_listener(), spawn(?MODULE, listen, [])}),
%%  data_manager:lookup(processes_set(), process_listener()).

stop() ->
  io:format("Process manager is stopped ~p~n", [registered()]),
  data_manager:delete_process(a,process_listener()).
%%  ets:delete(processes_set()),
%%  ets:new(processes_set(), [ordered_set, public, named_table]),


close_app() ->
  ets:delete(states),
  data_manager:delete_process(a,guiPID),
  close.

listen() ->
  receive
    {create, Key, Pid} -> io:format("Creating process in container ~s at id ~s~p~n", [processes_set(), Key,Pid]),
      data_manager:create_process(processes_set(),Key, Pid),
      listen();
    {delete, Key} -> io:format("Deleting process from container ~s at id ~s~n", [processes_set(), Key]),
      data_manager:delete_process(processes_set(),Key),
      io:format("Deleted process from container ~s at id ~s~n", [processes_set(), Key]),
      listen();
    {stop} -> io:format("Ending storing in ~s", [processes_set()]), data_manager:close(processes_set()),
      listen()
  end.

close() ->
  io:format("Stopping listener ~n"), exit(data_manager:lookup(processes_set(), process_listener()), stop),
  io:format("Stopping orchestrator ~n"), exit(pid(), stop).


