-module(burglary_alarm).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(logger_manager:logger_listener()).

sensor_controller_listener_PID() ->
  data_manager:lookup(sensor_controller:sensor_listener()).

signal_emission_timeout() -> timer:sleep(timer:seconds(6)).

pid() -> self().

name() -> burglary_alarm.

run() ->
  try
    io:format("Starting burglary system ~n"),
    process_listener_PID() ! {create, burglary_alarm},
    Id = list_to_atom("burglary_alarm_sensor_" ++ integer_to_list(data_manager:lookup_state(alarm_counter))),
    Pid1 = spawn(fun() -> burglary_sensor(Id) end),
    process_listener_PID() ! {create, Id, Pid1},
    data_manager:update_alarm_counter(),
    io:format("Starting burglary sensor~n"),

    start
  catch
    A:B -> io:format("~s~s~n", [A, B]),
      logger_PID() ! {burglary_alarm_sensor, "Error while creating bulglary sensor ~n"},
      error
  end.

run_new_burglary_sensor(Id) ->
  Name = list_to_atom("burglary_alarm_sensor_" ++ integer_to_list(Id)),
  Pid1 = spawn(fun() -> burglary_sensor(Name) end),
  process_listener_PID() ! {create, Name, Pid1},
  data_manager:update_alarm_counter(),
  io:format("Starting burglary sensor ~p~n", [Id]),
  start.

terminate() ->
  try
    io:format("Stopping burglary system ~n"),
      NumberOfSensors = data_manager:lookup_state(alarm_counter) - 1,
      delete_burglary_sensor(NumberOfSensors)
  catch
    A:B -> io:format("~s~s~n", [A, B]),
      logger_PID() ! {burglary_alarm_sensor, "Error while stopping burglary system ~n"},
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


delete_burglary_sensor(0) -> io:format("Burglary sensors deleted");

delete_burglary_sensor(NumberOfSensors)->
  Name = list_to_atom("burglary_alarm_sensor_" ++ integer_to_list(NumberOfSensors)),
  process_listener_PID() ! {delete, Name},
  delete_burglary_sensor(NumberOfSensors-1).

