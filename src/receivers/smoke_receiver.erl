-module(smoke_receiver).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

receiver_id() -> smoke_receiver_listener.

run() ->
  try
    io:format("Starting smoke receiver ~n"),
    process_listener_PID() ! {create, smoke_receiver, self()},
    ListenerPID = invoke_receiver(),
    io:format("Starting smoke receiver listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, smoke_receiver_listener, ListenerPID},
    start
  catch
    _:_ -> logger_PID() ! {smoke_receiver, "Error while creating smoke receiver"},
      error
  end.

terminate() ->
  try
    io:format("Stopping smoke receiver ~n"),
    process_listener_PID() ! {delete, process_orchestrator:processes_set(), smoke_receiver},
    process_listener_PID() ! {delete, process_orchestrator:processes_set(), smoke_receiver_listener}
  catch
    error:_ -> logger_PID() ! {"Error while terminating alarm!"},
      error
  end.

invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, smoke_sensor_receiver(), []) end).

smoke_sensor_receiver() ->
  receive
    {on} ->
      io:format("Smoke sensor received smoke alert!~n"),
      logger_PID() ! {smoke_receiver, "Smoke sensor received smoke alert!"},
      smoke_sensor_receiver();
    {_} ->
      smoke_sensor_receiver()
  end.



