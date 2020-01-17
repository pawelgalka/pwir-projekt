-module(sensor_controller).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

proxy_controller_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), signal_proxy:signal_proxy()).

sensor_listener() -> sensor_controller_listener.

run() ->
  try
    io:format("Starting sensor controller ~n"),
    process_listener_PID() ! {create, sensor_controller, self()},
    ListenerPID = invoke_receiver(),
    io:format("Starting sensor controller listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, sensor_controller_listener, ListenerPID},
    io:format("Creating sensors~n"),
    sensor_database:init_sensors(),
    timer:sleep(timer:seconds(1)),
    start
  catch
    _:_ -> logger_PID() ! {sensor_controller, "Error while creating sensor controller ~n"},
      error
  end.


invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, sensor_controller_receiver(), []) end).

sensor_controller_receiver() ->
  receive
    {Name, Data} ->
      io:format("Controller received data ~p ~p~n", [Name, Data]),
      proxy_controller_PID() ! {Name, Data},
      sensor_controller_receiver();
    {_} ->
      sensor_controller_receiver()
  end.

