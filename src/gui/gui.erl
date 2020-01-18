%%%-------------------------------------------------------------------
%%% @author micha
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. sty 2020 12:28
%%%-------------------------------------------------------------------
-module(gui).
-author("micha").

%% API
-compile(export_all).

gui() ->
  P_PID = self(),
  io:format("GUI is initialized ~p~n", [P_PID]),
  ets:insert(process_orchestrator:processes_set(), {guiPID, P_PID}),

  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Smart Erlang Community Home"),
  Panel = wxPanel:new(Frame),

  createLabels(Panel),
  BlindsText = wxStaticText:new(Panel, 14, "Up", [{pos, {156, 50}}]),
  ClimateText = wxStaticText:new(Panel, 14, "Off", [{pos, {210, 50}}]),
  OutletText = wxStaticText:new(Panel, 14, "No", [{pos, {257, 50}}]),
  SmokeText = wxStaticText:new(Panel, 14, "No", [{pos, {310, 50}}]),

  StartButton = wxButton:new(Panel, 12, [{label, "START"}]),
  StopButton = wxButton:new(Panel, 12, [{label, "STOP"}, {pos, {0, 50}}]),

  wxButton:connect(StartButton, command_button_clicked, [{callback,
    fun(_, _) -> P_PID ! start end}]),
  wxButton:connect(StopButton, command_button_clicked, [{callback,
    fun(_, _) -> P_PID ! stop end}]),
  wxFrame:show(Frame),

  awaitStart(BlindsText, ClimateText, OutletText, SmokeText).

createLabels(Panel) ->
  BlindsLabel = wxStaticText:new(Panel, 14, "Blinds", [{pos, {150, 20}}]),
  ClimateLabel = wxStaticText:new(Panel, 14, "Climate", [{pos, {200, 20}}]),
  OutletLabel = wxStaticText:new(Panel, 14, "Outlet", [{pos, {250, 20}}]),
  SmokeLabel = wxStaticText:new(Panel, 14, "Smoke", [{pos, {300, 20}}]).

awaitStart(BlindsText, ClimateText, OutletText, SmokeText) ->
  receive
    start -> app_warmup:initiate_app()
  end,
  awaitCommand(BlindsText, ClimateText, OutletText, SmokeText).

awaitCommand(BlindsText, ClimateText, OutletText, SmokeText) ->
  receive
    blindDown -> wxStaticText:setLabel(BlindsText, "Down");

    blindUp -> wxStaticText:setLabel(BlindsText, "Up");

    climateOn -> wxStaticText:setLabel(ClimateText, "On");

    climateOff -> wxStaticText:setLabel(ClimateText, "Off");

    outletOn -> wxStaticText:setLabel(OutletText, "On");

    outletOff -> wxStaticText:setLabel(OutletText, "Off");

    smokeOn -> wxStaticText:setLabel(SmokeText, "Yes");

    smokeOff -> wxStaticText:setLabel(SmokeText, "No");

  %% TODO: consider phone notification & temperature on GUI

  %% TODO: light button for electric outlet
    stop -> app_warmup:terminate_app()
  end,
  awaitCommand(BlindsText, ClimateText, OutletText, SmokeText).