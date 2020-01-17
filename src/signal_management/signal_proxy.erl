-module(signal_proxy).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

receiver_controller_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), receiver_controller:listener()).

signal_proxy() -> proxy_listener.

run() ->
  try
    io:format("Starting signal proxy ~n"),
    process_listener_PID() ! {create, proxy, self()},
    ListenerPID = invoke_receiver(),
    io:format("Starting proxy listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, proxy_listener, ListenerPID},
    start
  catch
    _:_ -> logger_PID() ! {sensor_controller, "Error while creating proxy"},
      error
  end.


invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, proxy_signal_receiver(), []) end).

proxy_signal_receiver() ->
  receive
    {Name, Data} ->
      io:format("Proxy received data ~p ~p~n", [Name, Data]),
      case Name of
        smoke_sensor -> receiver_controller_PID() ! {[smoke_receiver],Data};
        temperature_sensor -> receiver_controller_PID() ! {[climate_control_receiver],Data}
%%      phone_notification
      end,
      proxy_signal_receiver();
    {_} ->
      proxy_signal_receiver()
  end.

