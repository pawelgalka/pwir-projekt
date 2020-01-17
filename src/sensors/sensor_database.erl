-module(sensor_database).

%% API
-compile(export_all).

init_sensors() ->
  spawn(fun() -> smoke_sensor:run() end).
