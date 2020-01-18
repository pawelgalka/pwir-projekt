-module(app_warmup).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

start_gui() ->
  io:format("PROGRAM STARTED~n"),
  process_orchestrator:init(),
  gui:gui().

initiate_app() ->
  process_orchestrator:invoke_listener(),

  spawn(fun() -> logger_manager:run() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> signal_proxy:run() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> receiver_controller:run() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> sensor_controller:run() end),
  timer:sleep(timer:seconds(1)).

terminate_app() ->
  %% TODO: fix termination
  spawn(fun() -> sensor_controller:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> receiver_controller:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> signal_proxy:terminate() end),
  timer:sleep(timer:seconds(1)).
