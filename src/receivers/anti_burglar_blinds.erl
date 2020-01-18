-module(anti_burglar_blinds).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

run() ->
  try
    io:format("Starting blind receiver ~n"),
    process_listener_PID() ! {create, blind_receiver, self()},
    ListenerPID1 = invoke_receiver(blind_receiver_listener1),
    io:format("Starting blind receiver listener 1 at PID : ~p ~n", [ListenerPID1]),
    process_listener_PID() ! {create, blind_receiver_listener1_state, up},
    process_listener_PID() ! {create, blind_receiver_listener1, ListenerPID1},
    ListenerPID2 = invoke_receiver(blind_receiver_listener2),
    io:format("Starting blind receiver listener 2 at PID : ~p ~n", [ListenerPID2]),
    process_listener_PID() ! {create, blind_receiver_listener2_state, up},
    process_listener_PID() ! {create, blind_receiver_listener2, ListenerPID2},
    start
  catch
    A:B -> io:format("~s~s~n", [A, B]), logger_PID() ! {blind_receiver, "Error while creating blind receiver ~n"},
      error
  end.

terminate() ->
  try
    io:format("Stopping blind receiver ~n"),
    process_listener_PID() ! {delete, process_orchestrator:processes_set(), blind_receiver},
    process_listener_PID() ! {delete, process_orchestrator:processes_set(), blind_receiver_listener1},
    process_listener_PID() ! {delete, process_orchestrator:processes_set(), blind_receiver_listener2}
  catch
    error:_ -> logger_PID() ! {blind_receiver, "Error while terminating blind receiver!~n"},
      error
  end.

invoke_receiver(Param) ->
  spawn(fun() -> spawn(?MODULE, blind_receiver(Param), []) end).

blind_receiver(Param) ->
  receive
    {down} ->
      process_orchestrator:gui_PID() ! blindDown,
      io:format("~p going down~n",[Param]),
      logger_PID() ! {blind_receiver, atom_to_list(Param) ++ " going down"},
      blind_receiver(Param);
    {up} ->
      process_orchestrator:gui_PID() ! blindUp,
      io:format("~p going up~n",[Param]),
      logger_PID() ! {blind_receiver, atom_to_list(Param) ++ " going up"},
      blind_receiver(Param);
    {_} ->
      blind_receiver(Param)
  end.