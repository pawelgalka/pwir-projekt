-module(gui).

%% API
-compile(export_all).

sensor_controller_listener_PID() ->
  data_manager:lookup(sensor_controller:sensor_listener()).

login() -> data_manager:lookup_state(login).
password() -> data_manager:lookup_state(password).

gui() ->
  GUI_PID = self(),
  io:format("GUI is initialized ~p~n", [GUI_PID]),
  data_manager:create_process(guiPID, GUI_PID),
  login_page(GUI_PID).

smart_home_gui() ->
  GUI_PID = self(),
  io:format("GUI smart home is initialized ~p~n", [GUI_PID]),
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Smart Erlang Community Home", [{size, {500, 600}}]),
  Panel = wxPanel:new(Frame),

  create_labels(Panel),
  Validation = wxStaticText:new(Panel, 16, "", [{pos, {200, 430}}, {size, {100, 25}}]),
  AlarmCounter = wxStaticText:new(Panel, 16, "Alarm Controller: 1", [{pos, {50, 490}}, {size, {130, 25}}]),

  {BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText} = create_info_fields(Panel),

  StartButton = wxButton:new(Panel, 20, [{label, "START"}, {pos, {40, 50}}, {size, {100, 25}}]),
  StopButton = wxButton:new(Panel, 20, [{label, "STOP"}, {pos, {180, 50}}, {size, {100, 25}}]),
  CloseButton = wxButton:new(Panel, 20, [{label, "CLOSE"}, {pos, {320, 50}}, {size, {100, 25}}]),
  LightButton = wxButton:new(Panel, 20, [{label, "SWITCH LIGHTS"}, {pos, {100, 270}}, {size, {100, 50}}]),
  ArmButton = wxButton:new(Panel, 20, [{label, "ARM ALARM"}, {pos, {300, 270}}, {size, {100, 50}}]),
  AddAlarmButton = wxButton:new(Panel, 20, [{label, "ADD ALARM"}, {pos, {300, 480}}, {size, {100, 50}}]),
  {ChoiceMin, ChoiceMax, Choices} = create_choice(Panel),

  wxButton:connect(CloseButton, command_button_clicked, [{callback,
    fun(_, _) -> GUI_PID ! close end}]),
  wxButton:connect(StartButton, command_button_clicked, [{callback,
    fun(_, _) -> GUI_PID ! start end}]),
  wxButton:connect(StopButton, command_button_clicked, [{callback,
    fun(_, _) -> GUI_PID ! stop end}]),

  wxButton:connect(LightButton, command_button_clicked, [{callback,
    fun(_, _) ->
      State = data_manager:lookup_state(light_state),
      if State == on ->
        sensor_controller_listener_PID() ! {light_swtich, off};
        true -> sensor_controller_listener_PID() ! {light_swtich, on}
      end
    end}]),
  wxButton:connect(ArmButton, command_button_clicked, [{callback,
    fun(_, _) ->
      State = data_manager:lookup(armed),
      if State == on ->
        wxButton:setLabel(ArmButton, "ARM ALARM"),
        sensor_controller_listener_PID() ! {armed, off};
        true -> wxButton:setLabel(ArmButton, "UNARM ALARM"), sensor_controller_listener_PID() ! {armed, on}
      end
    end}]),
  wxButton:connect(AddAlarmButton, command_button_clicked, [{callback,
    fun(_, _) -> spawn(fun() ->
      burglary_alarm:run_new_burglary_sensor(data_manager:lookup_state(alarm_counter)) end),
      wxStaticText:setLabel(AlarmCounter, "Alarm Controller: " ++ integer_to_list(data_manager:lookup_state(alarm_counter)))
    end}]),

  wxFrame:show(Frame),
  wxChoice:connect(ChoiceMax, command_choice_selected, [{callback, fun(_, _) ->
    {Temp, _} = string:to_float(lists:nth(wxChoice:getSelection(ChoiceMax) + 1, Choices)),
    {Result} = validateTempRange(data_manager:lookup_state(minTemp), Temp),
    if Result == ok -> wxStaticText:setLabel(Validation, ""), data_manager:create_process(maxTemp, Temp); true ->
      wxStaticText:setLabel(Validation, "INVALID TEMP RANGE")
    end end}]),

  wxChoice:connect(ChoiceMin, command_choice_selected, [{callback, fun(_, _) ->
    {Temp, _} = string:to_float(lists:nth(wxChoice:getSelection(ChoiceMin) + 1, Choices)),
    {Result} = validateTempRange(Temp, data_manager:lookup_state(maxTemp)),
    io:format("Temp ~p", [Temp]),
    if Result == ok -> wxStaticText:setLabel(Validation, ""), data_manager:create_process(minTemp, Temp); true ->
      wxStaticText:setLabel(Validation, "INVALID TEMP RANGE")
    end end}]),

  await_start(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText, AlarmCounter).

validateTempRange(T1, T2) ->
  if T1 < T2 -> {ok};
    true -> {error}
  end.

create_choice(Panel) ->
  Choices = ["18.5", "19.0", "19.5", "20.0", "20.5", "21.0", "21.5", "22.0", "22.5", "23.0", "23.5", "24.0", "24.5", "25.0", "25.5", "26.0"],
  ChoiceMax = wxChoice:new(Panel, 20, [{pos, {300, 390}}, {size, {100, 50}}, {choices, Choices}]),
  ChoiceMin = wxChoice:new(Panel, 20, [{pos, {100, 390}}, {size, {100, 50}}, {choices, Choices}]),
  wxChoice:setColumns(ChoiceMax, [{n, 16}]),
  wxChoice:setColumns(ChoiceMin, [{n, 16}]),
  wxChoice:setSelection(ChoiceMax, last_max_temp()),
  wxChoice:setSelection(ChoiceMin, last_min_temp()),
  {ChoiceMin, ChoiceMax, Choices}.

last_min_temp() ->
  trunc((climate_control:minTemp() - 18.5) * 2).

last_max_temp() ->
  trunc((climate_control:maxTemp() - 18.5) * 2).

create_info_fields(Panel) ->
  BlindsText1 = wxStaticText:new(Panel, 14, "Up", [{pos, {50, 150}}]),
  BlindsText2 = wxStaticText:new(Panel, 14, "Up", [{pos, {135, 150}}]),
  ClimateText = wxStaticText:new(Panel, 14, "Off", [{pos, {220, 150}}]),
  OutletText = wxStaticText:new(Panel, 14, "Off", [{pos, {305, 150}}]),
  SmokeText = wxStaticText:new(Panel, 14, "No", [{pos, {390, 150}}]),
  TempText = wxStaticText:new(Panel, 14, "22.0 °C", [{pos, {250, 190}}]),
  PhoneText = wxStaticText:new(Panel, 14, "[SMS]", [{pos, {70, 230}}]),
  {BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText}.

login_page(GUI_PID) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Login", [{size, {300, 300}}]),
  PanelLogin = wxPanel:new(Frame),

  wxStaticText:new(PanelLogin, 16, "Login:   ", [{pos, {50, 50}}, {size, {60, 25}}]),
  wxStaticText:new(PanelLogin, 16, "Password:", [{pos, {50, 100}}, {size, {60, 25}}]),

  Result = wxStaticText:new(PanelLogin, 16, "", [{pos, {20, 200}}]),
  Login = wxTextCtrl:new(PanelLogin, 1, [{pos, {120, 50}}, {size, {100, 25}}]),
  Password = wxTextCtrl:new(PanelLogin, 1, [{pos, {120, 100}}, {size, {100, 25}}]),

  LoginButton = wxButton:new(PanelLogin, 20, [{label, "LOGIN"}, {pos, {100, 150}}, {size, {100, 25}}]),
  wxButton:connect(LoginButton, command_button_clicked, [{callback,
    fun(_, _) -> GUI_PID ! {wxTextCtrl:getValue(Login), wxTextCtrl:getValue(Password)} end}]),

  wxFrame:show(Frame),
  handle_login(Result, Frame).

handle_login(Result, FrameLogin) ->
  receive
    {Login, Password} ->
      LoginCorrect = string:equal(Login, login()),
      PasswordCorrect = string:equal(Password, password()),
      if LoginCorrect and PasswordCorrect ->
        wxStaticText:setLabel(Result, "Login successful. You will be redirected to app."),
        timer:sleep(timer:seconds(1)),
        wxFrame:destroy(FrameLogin),
        wx:destroy(),
        smart_home_gui();
        true -> wxStaticText:setLabel(Result, "Login failed! Please try again.")
      end
  end,
  handle_login(Result, FrameLogin).

create_labels(Panel) ->
  wxStaticText:new(Panel, 16, "Blind no. 1", [{pos, {50, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Blind no. 2", [{pos, {135, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Climate", [{pos, {220, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Outlet", [{pos, {305, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "Smoke", [{pos, {390, 125}}, {size, {60, 25}}]),
  wxStaticText:new(Panel, 16, "House temperature:", [{pos, {140, 190}}, {size, {130, 25}}]),
  wxStaticText:new(Panel, 16, "Minimal temperature:", [{pos, {100, 350}}, {size, {130, 25}}]),
  wxStaticText:new(Panel, 16, "Maximum temperature:", [{pos, {300, 350}}, {size, {130, 25}}]).

await_start(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText, AlarmCounter) ->
  receive
    start -> app_warmup:initiate_app();
    close -> wx:destroy(), process_orchestrator:close_app()
  end,
  await_command(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText, AlarmCounter).

await_command(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText, AlarmCounter) ->
  receive
    {blind_receiver_listener1, blindDown} -> wxStaticText:setLabel(BlindsText1, "Down");

    {blind_receiver_listener1, armed} -> wxStaticText:setLabel(BlindsText1, "Armed");

    {blind_receiver_listener1, blindUp} -> wxStaticText:setLabel(BlindsText1, "Up");

    {blind_receiver_listener2, blindDown} -> wxStaticText:setLabel(BlindsText2, "Down");

    {blind_receiver_listener2, armed} -> wxStaticText:setLabel(BlindsText2, "Armed");

    {blind_receiver_listener2, blindUp} -> wxStaticText:setLabel(BlindsText2, "Up");

    {temp, Temp} -> wxStaticText:setLabel(TempText, io_lib:format("~.1f °C", [Temp]));

    {phone, Notification} -> wxStaticText:setLabel(PhoneText, Notification);

    {climateOn} -> wxStaticText:setLabel(ClimateText, "On");

    {climateOff} -> wxStaticText:setLabel(ClimateText, "Off");

    {outletOn} -> wxStaticText:setLabel(OutletText, "On");

    {outletOff} -> wxStaticText:setLabel(OutletText, "Off");

    {smokeOn} -> wxStaticText:setLabel(SmokeText, "Yes");

    {smokeOff} -> wxStaticText:setLabel(SmokeText, "No");

    stop -> app_warmup:terminate_app(), wxStaticText:setLabel(AlarmCounter, "Alarm Controller: 1");

    start -> app_warmup:initiate_app();

    close -> wx:destroy(), process_orchestrator:close_app();

    Command -> io:format("Unknown signal ~p~n", [Command])
  end,
  await_command(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText, AlarmCounter).

bin_to_num(Bin) ->
  N = binary_to_list(Bin),
  case string:to_float(N) of
    {error, no_float} -> list_to_integer(N);
    {F, _Rest} -> F
  end.