-module(logger_manager).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

logger_listener() -> logger_listener.

run() ->
  try
    io:format("Starting logger ~n"),
    ListenerPID = invoke_receiver(),
    process_listener_PID() ! {create, logger_listener, ListenerPID},
    io:format("Logger listener  started at PID : ~p ~n", [ListenerPID]),
    start
  catch
    A:B -> io:format("Error while creating logger ~s~s~n",[A,B]),
      error
  end.

terminate() ->
  try
    io:format("Stopping logger ~n"),
    process_listener_PID() ! {delete, logger_listener}
  catch
    error:_ -> log(logger,"Error while terminating logger!"),
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