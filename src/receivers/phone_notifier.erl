-module(phone_notifier).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(logger_manager:logger_listener()).

receiver_id() -> phone_notifier_listener.

run() ->
  try
    io:format("Starting notifier ~n"),
    ListenerPID = invoke_receiver(),
    io:format("Starting notifier listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, phone_notifier_listener, ListenerPID},
    start
  catch
    _:_ -> logger_PID() ! {phone_notifier, "Error while creating notifier"},
      error
  end.

terminate() ->
  try
    io:format("Stopping notifier ~n"),
    process_listener_PID() ! {delete, phone_notifier_listener}
  catch
    error:_ -> logger_PID() ! {"Error while terminating notifier!"},
      error
  end.

invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, phone_notifier_receiver(), []) end).

phone_notifier_receiver() ->
  receive
    {smoke} ->
      io:format("[SMS] House is on fire~n"),
      logger_PID() ! {phone_notifier, "[SMS] House is on fire"},
      process_orchestrator:gui_PID() ! {phone, "[SMS] House is on fire"},
      phone_notifier_receiver();
    {breach,Sensor} ->
      io:format("[SMS] Someone is breaking to your house~n"),
      logger_PID() ! {phone_notifier, "[SMS] Someone is breaking to your house from "++atom_to_list(Sensor)},
      [_, _, _, Id] = string:tokens(atom_to_list(Sensor), "_"),
      process_orchestrator:gui_PID() ! {phone, "[SMS] Someone is breaking to your house from burglary alarm sensor "++Id},
      phone_notifier_receiver();
    {_} ->
      phone_notifier_receiver()
  end.
