-module(receivers_database).

%% API
-compile(export_all).

init_receivers() ->
  spawn(fun() -> smoke_receiver:run() end),
  spawn(fun() -> electrical_outlet_receiver:run() end),
  spawn(fun() -> climate_control:run() end).
