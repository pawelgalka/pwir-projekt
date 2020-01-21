-module(electrical_outlet_receiver).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(logger_manager:logger_listener()).

receiver_id() -> electrical_outlet_receiver_listener.

run() ->
  try
    io:format("Starting electrical outlet receiver ~n"),
    ListenerPID = invoke_receiver(),
    io:format("Starting electrical outlet receiver listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, electrical_outlet_receiver_listener, ListenerPID},
    start
  catch
    _:_ -> logger_PID() ! {electrical_outlet_receiver, "Error while creating electrical outlet receiver ~n"},
      error
  end.

terminate() ->
  try
    io:format("Stopping electrical outlet receiver ~n"),
    process_listener_PID() ! {delete, electrical_outlet_receiver_listener}
  catch
    error:_ -> logger_PID() ! {electrical_outlet_receiver, "Error while terminating electrical outlet receiver!~n"},
      error
  end.

invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, electric_sensor_receiver(), []) end).

electric_sensor_receiver() ->
  receive
    {on} ->
      io:format("Turning electrical outlet on! ~n"),
      process_orchestrator:gui_PID() ! {outletOn},
      logger_PID() ! {electrical_outlet_receiver, "Turning electrical outlet on!"},
      electric_sensor_receiver();
    {off} ->
      io:format("Turning electrical outlet off! ~n"),
      process_orchestrator:gui_PID() ! {outletOff},
      logger_PID() ! {electrical_outlet_receiver, "Turning electrical outlet off!"},
      electric_sensor_receiver();
    {_} ->
      electric_sensor_receiver()
  end.

