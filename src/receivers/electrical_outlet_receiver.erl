-module(electrical_outlet_receiver).

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
    io:format("Starting electrical outlet receiver ~n"),
    process_listener_PID() ! {create, electrical_outlet_receiver, self()},
    ListenerPID = invoke_receiver(),
    io:format("Starting electrical outlet receiver listener at PID : ~p ~n",[ListenerPID]),
    process_listener_PID() ! {create, electrical_outlet_receiver_listener, ListenerPID},
    start
  catch
    _:_ -> logger_PID() ! {electrical_outlet_receiver, "Error while creating electrical outlet receiver ~n"},
      error
  end.

terminate() ->
  try
    io:format("Stopping electrical outlet receiver ~n"),
    process_listener_PID() ! {delete,process_orchestrator:processes_set(), electrical_outlet_receiver},
    process_listener_PID() ! {delete,process_orchestrator:processes_set(), electrical_outlet_receiver_listener}
  catch
    error:_ -> logger_PID() ! {electrical_outlet_receiver,"Error while terminating electrical outlet receiver!~n"},
      error
  end.

invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, smoke_sensor_receiver(), []) end).

smoke_sensor_receiver() ->
  receive
    {on} ->
      io:format("Turning elecricatl outlet on!"),
      logger_PID() ! {electrical_outlet_receiver,"Turning elecricatl outlet on!"},
      smoke_sensor_receiver();
    {off} ->
      io:format("Turning elecricatl outlet off!"),
      logger_PID() ! {electrical_outlet_receiver,"Turning elecricatl outlet off!"},
      smoke_sensor_receiver();
    {_} ->
      smoke_sensor_receiver()
  end.

