-module(logger_manager).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_listener() -> logger_listener.

run() ->
  try
    io:format("Starting logger ~n"),
    process_listener_PID() ! {create, logger, pid()},
    ListenerPID = invoke_receiver(),
    io:format("Starting logger listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, logger_listener, ListenerPID},
    start
  catch
    A:B -> io:format("Error while creating logger ~s~s~n",[A,B]),
      error
  end.

terminate() ->
  try
    io:format("Stopping smoke receiver ~n"),
    process_listener_PID() ! {delete, process_orchestrator:processes_set(), smoke_receiver},
    process_listener_PID() ! {delete, process_orchestrator:processes_set(), smoke_receiver_listener}
  catch
    error:_ -> log(logger,"Error while terminating alarm!"),
      error
  end.

invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, logger_receiver(), []) end).

logger_receiver() ->
  receive
    {Module,Line} ->
      log(Module,Line),
      logger_receiver();
    {_} ->
      log(logger,"Invalid data received."),
      logger_receiver()
  end.

log(Module,Line) ->
  {ok, Log} = file:open("log.txt", [append]),
  {H,M,S} = time(),
  io:format(Log, "[~s] : ~2..0b:~2..0b:~2..0b : ~s~n", [Module,H,M,S,Line]),
  file:close(Log).