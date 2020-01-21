-module(signal_proxy).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(logger_manager:logger_listener()).

receiver_controller_PID() ->
  data_manager:lookup(receiver_controller:listener()).

signal_proxy() -> proxy_listener.

run() ->
  try
    io:format("Starting signal proxy ~n"),
    io:format("~p ~n",[self()]),
    ListenerPID = invoke_receiver(),
    io:format("Starting proxy listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, proxy_listener, ListenerPID},
    start
  catch
    _:_ -> logger_PID() ! {proxy, "Error while creating proxy"},
      error
  end.

terminate() ->
  try
    io:format("Stopping signal proxy ~n"),
    process_listener_PID() ! {delete, proxy_listener},
    stop
  catch
    _:_ -> logger_PID() ! {proxy, "Error while stopping proxy"},
      error
  end.


invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, proxy_signal_receiver(), []) end).

proxy_signal_receiver() ->
  receive
    {Name, Data} ->
      io:format("Proxy received data ~p ~p~n", [Name, Data]),
      case Name of
        smoke_sensor -> receiver_controller_PID() ! {[smoke_receiver,phone_notifier],Data};
        temperature_sensor -> receiver_controller_PID() ! {[climate_control_receiver],Data};
        light_swtich -> receiver_controller_PID() ! {[electrical_outlet_receiver],Data};
        armed -> receiver_controller_PID() ! {[arming],Data};
        BurglarySensor -> receiver_controller_PID() ! {[security,phone_notifier],{Data,BurglarySensor}} %%one of alarms
      end,
      proxy_signal_receiver();
    {_} ->
      proxy_signal_receiver()
  end.

