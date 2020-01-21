-module(app_warmup).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

start_gui() ->
  process_orchestrator:init(),
  initialize_data(),
  io:format("PROGRAM STARTED~n"),
  spawn(fun() -> gui:gui() end),
  started.

initialize_data() ->
  {ok, Data} = file:read_file("init_states"),
  [Login, Password, MinTemp, MaxTemp] = string:tokens(erlang:binary_to_list(Data), "\r\n"),
  {MinTempValue, _} = string:to_float(MinTemp),
  {MaxTempValue, _} = string:to_float(MaxTemp),
  data_manager:create_process(process_orchestrator:processes_set(), login, Login),
  data_manager:create_process(process_orchestrator:processes_set(), password, Password),
  data_manager:create_process(process_orchestrator:processes_set(), minTemp, MinTempValue),
  data_manager:create_process(process_orchestrator:processes_set(), maxTemp, MaxTempValue).

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
  spawn(fun() -> sensor_controller:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> receiver_controller:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> signal_proxy:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> logger_manager:terminate() end),
  timer:sleep(timer:seconds(1)),

  process_orchestrator:stop().
