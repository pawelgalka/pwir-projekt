-module(sensor_database).

%% API
-compile(export_all).

init_sensors() ->
  ets:insert(process_orchestrator:states_set(), {light_state, off}),
  ets:insert(process_orchestrator:states_set(), {armed, off}),
  prepare_alarm(),
  spawn(fun() -> smoke_sensor:run() end),
  spawn(fun() -> temperature_sensor:run() end),
  timer:sleep(timer:seconds(1)).

prepare_alarm() ->
  data_manager:create_alarm_counter(),
  spawn(fun() -> burglary_alarm:run() end).

terminate_sensors() ->
  spawn(fun() -> smoke_sensor:terminate() end),
  spawn(fun() -> temperature_sensor:terminate() end),
  spawn(fun() -> burglary_alarm:terminate() end).