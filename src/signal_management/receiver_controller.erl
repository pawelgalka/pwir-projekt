-module(receiver_controller).

%% API
-compile(export_all).

%% returns PID of receiver
pid() -> self().

process_listener_PID() ->
  data_manager:lookup(process_orchestrator:process_listener()).

logger_PID() ->
  data_manager:lookup(logger_manager:logger_listener()).

listener() -> receiver_controller_listener.

security_list() ->
  [{blind_receiver_listener1_state, blind_receiver_listener1}, {blind_receiver_listener2_state, blind_receiver_listener2}].

run() ->
  try
    io:format("Starting receiver controller ~n"),
    ListenerPID = invoke_receiver(),
    process_listener_PID() ! {create, receiver_controller_listener, ListenerPID},
    io:format("Started receiver controller listener at PID : ~p ~n", [ListenerPID]),
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
    io:format("Stopping receiver controller listener ~n"),
    process_listener_PID() ! {delete, receiver_controller_listener},
    io:format("Stopping receivers~n"),
    receivers_database:terminate_receivers(),
    timer:sleep(timer:seconds(1)),
    start
  catch
    _:_ -> logger_PID() ! {receiver_controller, "Error while stopping receiver controller ~n"},
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
    smoke_receiver -> spawn(?MODULE, handle_smoke_signal, [Data]);
    phone_notifier -> spawn(?MODULE,handle_notification, [Data]);
    climate_control_receiver -> spawn(?MODULE,handle_temperature_signal, [Data]);
    security -> spawn(?MODULE,handle_breach_signal, [Data]);
    arming -> spawn(?MODULE,handle_arming_signal,[security_list(), Data]);
    electrical_outlet_receiver -> spawn(?MODULE,handle_light_signal,[Data]);
    _ -> nic
  end,
  handleRequest(T, Data).

handle_smoke_signal(Data) ->
  case Data of
    true -> data_manager:lookup(smoke_receiver:receiver_id()) ! {on};
    false -> data_manager:lookup(smoke_receiver:receiver_id()) ! {off}
  end.

handle_light_signal(Data) ->
  case Data of
    on -> ets:delete(process_orchestrator:states_set(), light_state),
      ets:insert(process_orchestrator:states_set(), {light_state, on}),
      data_manager:lookup(electrical_outlet_receiver:receiver_id()) ! {on};
    off -> ets:delete(process_orchestrator:states_set(), light_state),
      ets:insert(process_orchestrator:states_set(), {light_state, off}),
      data_manager:lookup(electrical_outlet_receiver:receiver_id()) ! {off}
  end.

handle_temperature_signal({State, Data}) ->
  data_manager:lookup(climate_control:receiver_id()) ! {State, Data}.

handle_notification(Data) ->
  case Data of
    true -> data_manager:lookup(phone_notifier:receiver_id()) ! {smoke};
    false -> skip;
    danger -> data_manager:lookup(phone_notifier:receiver_id()) ! {breach};
    safe -> skip
  end.

handle_breach_signal(Data) ->
  Receivers = security_list(),
  io:format("Armed state: ~p~n",[data_manager:lookup_state(armed)]),
  handle_single_breach_signal(Receivers, Data, data_manager:lookup_state(armed)).

handle_single_breach_signal([], _, _) -> ok;

handle_single_breach_signal([{State, Receiver} | T], Data, ArmedState) when ArmedState == off ->
  ReceiverState = data_manager:lookup_state(State),
  ReceiverPID = data_manager:lookup(Receiver),
  io:format("~p ~p~n", [Receiver, ReceiverState]),
  case ReceiverState of
    up ->
      case Data of
        danger -> ReceiverPID ! {down},
          ets:delete(process_orchestrator:states_set(), State),
          ets:insert(process_orchestrator:states_set(), {State, down});
        safe -> ok
      end;
    down ->
      case Data of
        danger -> ok;
        safe -> ReceiverPID ! {up},
          ets:delete(process_orchestrator:states_set(), State),
          ets:insert(process_orchestrator:states_set(), {State, up})
      end
  end,
  handle_single_breach_signal(T, Data, ArmedState);

handle_single_breach_signal(_, _, ArmedState) when ArmedState == on -> ok.

handle_arming_signal(Receivers, Data) ->
  ArmedState = data_manager:lookup_state(armed),
  handle_single_arm_signal(Receivers, Data, ArmedState).

handle_single_arm_signal([], _, _) -> ok;

handle_single_arm_signal([{State, Receiver} | T], Data, ArmedState) ->
  ReceiverState = data_manager:lookup_state(State),
  ReceiverPID = data_manager:lookup(Receiver),
  case ArmedState of
    on ->
      ets:delete(process_orchestrator:states_set(), armed),
      ets:insert(process_orchestrator:states_set(), {armed, off}),
      ReceiverPID ! {ReceiverState};
    off ->
      ets:delete(process_orchestrator:states_set(), armed),
      ets:insert(process_orchestrator:states_set(), {armed, on}),
      ReceiverPID ! {armed}
  end,
  handle_single_arm_signal(T, Data, ArmedState).
