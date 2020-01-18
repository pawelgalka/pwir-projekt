-module(temperature_sensor).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

sensor_controller_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), sensor_controller:sensor_listener()).

signal_emission_timeout() -> timer:sleep(timer:seconds(6)).

name() -> temperature_sensor.

start_temperature() -> 22.0.

run() ->
  try
    io:format("Starting temperature sensor ~n"),
    process_listener_PID() ! {create, temperature_sensor, self()},
    Pid = spawn(fun() -> temperature_sensor_receiver() end),
    process_listener_PID() ! {create, temperature_sensor_receiver, Pid},
    Pid ! {on, start_temperature()},
    start
  catch
    A:B -> io:format("~s~s~n", [A, B]),
      logger_PID() ! {smoke_sensor, "Error while creating temperature receiver ~n"},
      error
  end.

temperature_sensor_receiver() ->
  receive
    {on, Temp} ->
      send_new_temperature_signal(on, Temp - 1),
      process_orchestrator:gui_PID() ! {temp, Temp - 1},
      signal_emission_timeout(),
      temperature_sensor_receiver();
    {off, Temp} ->
      send_new_temperature_signal(off, Temp + 0.5),
      process_orchestrator:gui_PID() ! {temp, Temp + 0.5},
      signal_emission_timeout(),
      temperature_sensor_receiver();
    {_, _} ->
      temperature_sensor_receiver()
  end.

send_new_temperature_signal(State, Temp) ->
  sensor_controller_listener_PID() ! {name(), {State, Temp}}.
