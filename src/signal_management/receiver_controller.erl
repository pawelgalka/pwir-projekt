-module(receiver_controller).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), logger_manager:logger_listener()).

listener() -> receiver_controller_listener.

run() ->
  try
    io:format("Starting receiver controller ~n"),
    process_listener_PID() ! {create, receiver_controller, self()},
    ListenerPID = invoke_receiver(),
    io:format("Starting receiver controller listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, receiver_controller_listener, ListenerPID},
    io:format("Creating receivers~n"),
    receivers_database:init_receivers(),
    start
  catch
    _:_ -> logger_PID() ! {sensor_controller, "Error while creating receiver controller ~n"},
      error
  end.


invoke_receiver() ->
  spawn(fun() -> spawn(?MODULE, receiver_controller(), []) end).

receiver_controller() ->
  receive
    {Receivers, Data} ->
      io:format("Receiver controller is handling request~n"),
      logger_PID() ! {receiver_controller, "Receiver controller is handling request"},
      handleRequest(Receivers, Data),
%%      phone_notification
      receiver_controller();
    {_} ->
      receiver_controller()
  end.



handleRequest([], _) ->
  io:format("Handling request finished~n"),
  logger_PID() ! {receiver_controller, "Handling request finished"};

handleRequest([H | T], Data) ->
  io:format("Handling request for receiver ~s~n", [H]),
  case H of
    smoke_receiver -> handle_smoke_signal(Data);
    _ -> nic
  end,
  handleRequest(T, Data).



handle_smoke_signal(Data) ->
  case Data of
    true -> data_manager:lookup(process_orchestrator:processes_set(), smoke_receiver:receiver_id()) ! {on};
    false -> data_manager:lookup(process_orchestrator:processes_set(), smoke_receiver:receiver_id()) ! {off}
  end.