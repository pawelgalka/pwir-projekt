-module(app_warmup).

%% API
-compile(export_all).

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

start_gui() ->
  process_orchestrator:init(),
  initialize_data(),
  io:format("PROGRAM STARTED~n"),
  spawn(fun() -> gui:gui() end),
  started.

initialize_data() ->
  {Login, Password} = resolve_user_data(),
  {MinTempValue, MaxTempValue} = resolve_last_data(),
  data_manager:create_process(login, Login),
  data_manager:create_process(password, Password),
  data_manager:create_process(minTemp, element(1, MinTempValue)),
  data_manager:create_process(maxTemp, element(1, MaxTempValue)).

resolve_user_data() ->
  {ok, Data} = file:read_file("user_data_base.txt"),
  [Login, Password] = string:tokens(erlang:binary_to_list(Data), "\r\n"),
  {Login, Password}.

resolve_last_data() ->
  {ok, Data} = file:read_file("init_states.txt"),
  [MinTemp, MaxTemp] = string:tokens(erlang:binary_to_list(Data), "\r\n"),
  {string:to_float(MinTemp), string:to_float(MaxTemp)}.

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
  data_manager:save_values(),
  spawn(fun() -> sensor_controller:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> receiver_controller:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> signal_proxy:terminate() end),
  timer:sleep(timer:seconds(1)),

  spawn(fun() -> logger_manager:terminate() end),
  timer:sleep(timer:seconds(1)),

  process_orchestrator:stop().
