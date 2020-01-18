-module(burglary_alarm).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

sensor_controller_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), sensor_controller:sensor_listener()).

signal_emission_timeout() -> timer:sleep(timer:seconds(6)).

pid() -> self().

name() -> burglary_alarm.

run() ->
  try
    io:format("Starting burglary system ~n"),
    process_listener_PID() ! {create, burglary_alarm, pid()},
    Pid1 = spawn(fun() -> burglary_sensor(burglary_alarm_sensor) end),
    process_listener_PID() ! {create, burglary_alarm_sensor, Pid1},
    io:format("Starting burglary sensor 1~n"),
    start
  catch
    A:B -> io:format("~s~s~n", [A, B]),
      logger_PID() ! {smoke_sensor, "Error while creating bulglary sensor ~n"},
      error
  end.

terminate() ->
  try
    io:format("Stopping burglary system ~n"),
    process_listener_PID() ! {delete, burglary_alarm_sensor},
    process_listener_PID() ! {delete, burglary_alarm}
  catch
    A:B -> io:format("~s~s~n", [A, B]),
      logger_PID() ! {smoke_sensor, "Error while stopping burglary system ~n"},
      error
  end.

burglary_sensor(Param) ->
  Random_output = rand:uniform(10),
  if
    Random_output >= 5 ->
      send_breach_signal(Param),
      signal_emission_timeout(),
      burglary_sensor(Param);
    true ->
      send_safe_signal(Param),
      signal_emission_timeout(),
      burglary_sensor(Param)
  end.


send_breach_signal(Param) ->
  sensor_controller_listener_PID() ! {Param, danger}.

send_safe_signal(Param) ->
  sensor_controller_listener_PID() ! {Param, safe}.
