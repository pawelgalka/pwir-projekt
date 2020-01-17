-module(app_warmup).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

initiate_app() ->
  process_orchestrator:init(),

  process_orchestrator:invoke_listener(),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> logger_manager:run() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> signal_proxy:run() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> receiver_controller:run() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> sensor_controller:run() end),
  timer:sleep(timer:seconds(1)).





