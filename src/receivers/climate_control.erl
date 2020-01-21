-module(climate_control).

%% API
-compile(export_all).

pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(logger_manager:logger_listener()).

temperature_sensor() -> data_manager:lookup(temperature_sensor_receiver).

minTemp() -> data_manager:lookup_state(minTemp).
maxTemp() -> data_manager:lookup_state(maxTemp).
receiver_id() -> climate_control_receiver_listener.

run() ->
  try
    io:format("Starting climate control receiver ~n"),
    ListenerPID = invoke_receiver(),
    process_listener_PID() ! {create, climate_control_receiver_listener, ListenerPID},
    io:format("Started climate control receiver listener at PID : ~p ~n", [ListenerPID]),
    start
  catch
    _:_ -> logger_PID() ! {climate_control_receiver, "Error while creating climate control receiver"},
      error
  end.

terminate() ->
  try
    io:format("Stopping climate control receiver ~n"),
    process_listener_PID() ! {delete, climate_control_receiver_listener}
  catch
    error:_ -> logger_PID() ! {"Error while terminating climate control!"},
      error
  end.

invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, climate_control_sensor_receiver(), []) end).

climate_control_sensor_receiver() ->
  receive
    {on, Temp} ->
      io:format("Climate control sensor received climate control on information! ~p~n", [Temp]),
      process_orchestrator:gui_PID() ! {climateOn},
      logger_PID() ! {climate_control_receiver, "Climate control sensor received climate control on information!"},
      MinTemp = minTemp(),
      if Temp < MinTemp ->
        temperature_sensor() ! {off, Temp},
        climate_control_sensor_receiver();
        true ->
          temperature_sensor() ! {on, Temp},
          climate_control_sensor_receiver()
      end;
    {off, Temp} ->
      io:format("Climate control sensor received climate control off information! ~p~n", [Temp]),
      process_orchestrator:gui_PID() ! {climateOff},
      logger_PID() ! {climate_control_receiver, "Climate control sensor received climate control off information!"},
      MaxTemp = maxTemp(),
      if Temp > MaxTemp ->
        temperature_sensor() ! {on, Temp},
        climate_control_sensor_receiver();
        true ->
          temperature_sensor() ! {off, Temp},
          climate_control_sensor_receiver()
      end;
    {_} ->
      climate_control_sensor_receiver()
  end.




