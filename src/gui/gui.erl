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
  Frame = wxFrame:new(Wx, -1, "Smart Erlang Community Home", [{size, {500, 500}}]),
  Panel = wxPanel:new(Frame),

  createLabels(Panel),
  BlindsText1 = wxStaticText:new(Panel, 14, "Up", [{pos, {50, 150}}, {size, {100, 25}}]),
  BlindsText2 = wxStaticText:new(Panel, 14, "Up", [{pos, {135, 150}}, {size, {100, 25}}]),
  ClimateText = wxStaticText:new(Panel, 14, "Off", [{pos, {220, 150}}, {size, {100, 25}}]),
  OutletText = wxStaticText:new(Panel, 14, "No", [{pos, {305, 150}}, {size, {100, 25}}]),
  SmokeText = wxStaticText:new(Panel, 14, "No", [{pos, {390, 150}}, {size, {100, 25}}]),
  TempText = wxStaticText:new(Panel, 14, "", [{pos, {250, 175}}, {size, {100, 25}}]),

  StartButton = wxButton:new(Panel, 20, [{label, "START"}, {pos, {120, 50}}, {size, {100, 25}}]),
  StopButton = wxButton:new(Panel, 20, [{label, "STOP"}, {pos, {280, 50}}, {size, {100, 25}}]),

  wxButton:connect(StartButton, command_button_clicked, [{callback,
    fun(_, _) -> P_PID ! start end}]),
  wxButton:connect(StopButton, command_button_clicked, [{callback,
    fun(_, _) -> P_PID ! stop end}]),
  wxFrame:show(Frame),

  awaitStart(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText).

createLabels(Panel) ->
  wxStaticText:new(Panel, 16, "Blind no. 1", [{pos, {50, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Blind no. 2", [{pos, {135, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Climate", [{pos, {220, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Outlet", [{pos, {305, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Smoke", [{pos, {390, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "House temperature", [{pos, {120, 175}}, {size, {130, 25}}]).

awaitStart(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText) ->
  receive
    start -> app_warmup:initiate_app()
  end,
  awaitCommand(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText).

awaitCommand(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText) ->
  receive
    {blind_receiver_listener1, blindDown} -> wxStaticText:setLabel(BlindsText1, "Down");

    {blind_receiver_listener1, blindUp} -> wxStaticText:setLabel(BlindsText1, "Up");

    {blind_receiver_listener2, blindDown} -> wxStaticText:setLabel(BlindsText2, "Down");

    {blind_receiver_listener2, blindUp} -> wxStaticText:setLabel(BlindsText2, "Up");

    {temp, Temp} -> wxStaticText:setLabel(TempText, float_to_list(Temp));

    climateOn -> wxStaticText:setLabel(ClimateText, "On");

    climateOff -> wxStaticText:setLabel(ClimateText, "Off");

    outletOn -> wxStaticText:setLabel(OutletText, "On");

    outletOff -> wxStaticText:setLabel(OutletText, "Off");

    smokeOn -> wxStaticText:setLabel(SmokeText, "Yes");

    smokeOff -> wxStaticText:setLabel(SmokeText, "No");

  %% TODO: consider phone notification & temperature on GUI

  %% TODO: light button for electric outlet
    stop -> app_warmup:terminate_app();

    _ -> doNothing
  end,
  awaitCommand(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText).