-module(receivers_database).

%% API
-compile(export_all).

init_receivers() ->
  spawn(fun() -> phone_notifier:run() end),
  spawn(fun() -> smoke_receiver:run() end),
  spawn(fun() -> electrical_outlet_receiver:run() end),
  spawn(fun() -> climate_control:run() end),
  spawn(fun() -> anti_burglar_blinds:run() end),
  timer:sleep(timer:seconds(1)).
