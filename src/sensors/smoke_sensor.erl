-module(smoke_sensor).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

sensor_controller_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), sensor_controller:sensor_listener()).

signal_emission_timeout() -> timer:sleep(timer:seconds(6)).

name() -> smoke_sensor.

run() ->
  try
    io:format("Starting smoke sensor ~n"),
    process_listener_PID() ! {create, smoke_sensor, self()},
    smoke_signal_exporter()
  catch
    A:B -> io:format("~s~s~n", [A, B]),
      logger_PID() ! {smoke_sensor, "Error while creating electrical outlet receiver ~n"},
      error
  end.


smoke_signal_exporter() ->
  Random_output = rand:uniform(10),
  if
    Random_output >= 5 ->
      send_smoke_detected_signal(),
      signal_emission_timeout(),
      smoke_signal_exporter();
    true ->
      send_smoke_not_detected_signal(),
      signal_emission_timeout(),
      smoke_signal_exporter()
  end.


send_smoke_not_detected_signal() ->
  sensor_controller_listener_PID() ! {name(), false}.


send_smoke_detected_signal() ->
  sensor_controller_listener_PID() ! {name(), true}.
