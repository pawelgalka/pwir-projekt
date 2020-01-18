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

security_list() ->
  [{blind_receiver_listener1_state, blind_receiver_listener1}, {blind_receiver_listener2_state, blind_receiver_listener2}].

run() ->
  try
    io:format("Starting receiver controller ~n"),
    process_listener_PID() ! {create, receiver_controller, self()},
    ListenerPID = invoke_receiver(),
    io:format("Starting receiver controller listener at PID : ~p ~n", [ListenerPID]),
    process_listener_PID() ! {create, receiver_controller_listener, ListenerPID},
    io:format("Creating receivers~n"),
    receivers_database:init_receivers(),
    timer:sleep(timer:seconds(1)),
    start
  catch
    _:_ -> logger_PID() ! {sensor_controller, "Error while creating receiver controller ~n"},
      error
  end.

terminate() ->
  try
    io:format("Stopping receiver controller ~n"),
    process_listener_PID() ! {delete, sensor_controller},
    io:format("Stopping receiver controller listener ~n"),
    process_listener_PID() ! {delete, sensor_controller_listener},
    io:format("Stopping receivers~n"),
    receivers_database:terminate_sensors(),
    timer:sleep(timer:seconds(1)),
    start
  catch
    _:_ -> logger_PID() ! {sensor_controller, "Error while stopping receiver controller ~n"},
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
    phone_notifier -> handle_notification(Data);
    climate_control_receiver -> handle_temperature_signal(Data);
    security -> handle_breach_signal(Data);
    electrical_outlet_receiver -> handle_light_signal(Data);
    _ -> nic
  end,
  handleRequest(T, Data).



handle_smoke_signal(Data) ->
  case Data of
    true -> data_manager:lookup(process_orchestrator:processes_set(), smoke_receiver:receiver_id()) ! {on};
    false -> data_manager:lookup(process_orchestrator:processes_set(), smoke_receiver:receiver_id()) ! {off}
  end.

handle_light_signal(Data) ->
  case Data of
    on -> ets:delete(process_orchestrator:processes_set(), light_state),
      ets:insert(process_orchestrator:processes_set(), {light_state, on}),
      data_manager:lookup(process_orchestrator:processes_set(), electrical_outlet_receiver:receiver_id()) ! {on};
    off ->ets:delete(process_orchestrator:processes_set(), light_state),
      ets:insert(process_orchestrator:processes_set(), {light_state, off}),
      data_manager:lookup(process_orchestrator:processes_set(), electrical_outlet_receiver:receiver_id()) ! {off}
  end.

handle_temperature_signal({State, Data}) ->
  data_manager:lookup(process_orchestrator:processes_set(), climate_control:receiver_id()) ! {State, Data}.


handle_notification(Data) ->
  case Data of
    true -> data_manager:lookup(process_orchestrator:processes_set(), phone_notifier:receiver_id()) ! {smoke};
    false -> skip;
    danger -> data_manager:lookup(process_orchestrator:processes_set(), phone_notifier:receiver_id()) ! {breach};
    safe -> skip
  end.

handle_breach_signal(Data) ->
  Receivers = security_list(),
  handle_single_breach_signal(Receivers, Data).


handle_single_breach_signal([], _) -> ok;

handle_single_breach_signal([{State, Receiver} | T], Data) ->
  ReceiverState = data_manager:lookup(process_orchestrator:processes_set(), State),
  ReceiverPID = data_manager:lookup(process_orchestrator:processes_set(), Receiver),
  io:format("~p ~p~n", [Receiver, ReceiverState]),
  case ReceiverState of
    up ->
      case Data of
        danger -> ReceiverPID ! {down},
          ets:delete(process_orchestrator:processes_set(), State),
          ets:insert(process_orchestrator:processes_set(), {State, down});
        safe -> ok
      end;
    down ->
      case Data of
        danger -> ok;
        safe -> ReceiverPID ! {up},
          ets:delete(process_orchestrator:processes_set(), State),
          ets:insert(process_orchestrator:processes_set(), {State, up})
      end
  end,
  handle_single_breach_signal(T, Data).

