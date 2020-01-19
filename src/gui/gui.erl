-module(gui).

%% API
-compile(export_all).

sensor_controller_listener_PID() ->
  data_manager:lookup(process_orchestrator:processes_set(), sensor_controller:sensor_listener()).

login() -> "admin".
password() -> "admin".

gui() ->
  GUI_PID = self(),
  io:format("GUI is initialized ~p~n", [GUI_PID]),
  ets:insert(process_orchestrator:processes_set(), {guiPID, GUI_PID}),
  login_page(GUI_PID).

smart_home_gui() ->
  GUI_PID = self(),
  io:format("GUI smart home is initialized ~p~n", [GUI_PID]),
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Smart Erlang Community Home", [{size, {500, 400}}]),
  Panel = wxPanel:new(Frame),

  create_labels(Panel),
  BlindsText1 = wxStaticText:new(Panel, 14, "Up", [{pos, {50, 150}}]),
  BlindsText2 = wxStaticText:new(Panel, 14, "Up", [{pos, {135, 150}}]),
  ClimateText = wxStaticText:new(Panel, 14, "Off", [{pos, {220, 150}}]),
  OutletText = wxStaticText:new(Panel, 14, "Off", [{pos, {305, 150}}]),
  SmokeText = wxStaticText:new(Panel, 14, "No", [{pos, {390, 150}}]),
  TempText = wxStaticText:new(Panel, 14, "22.0 °C", [{pos, {250, 190}}]),
  PhoneText = wxStaticText:new(Panel, 14, "[SMS]", [{pos, {160, 230}}]),

  StartButton = wxButton:new(Panel, 20, [{label, "START"}, {pos, {40, 50}}, {size, {100, 25}}]),
  StopButton = wxButton:new(Panel, 20, [{label, "STOP"}, {pos, {180, 50}}, {size, {100, 25}}]),
  CloseButton = wxButton:new(Panel, 20, [{label, "CLOSE"}, {pos, {320, 50}}, {size, {100, 25}}]),
  LightButton = wxButton:new(Panel, 20, [{label, "SWITCH LIGHTS"}, {pos, {200, 270}}, {size, {100, 50}}]),

  wxButton:connect(CloseButton, command_button_clicked, [{callback,
    fun(_, _) -> GUI_PID ! close end}]),
  wxButton:connect(StartButton, command_button_clicked, [{callback,
    fun(_, _) -> GUI_PID ! start end}]),
  wxButton:connect(StopButton, command_button_clicked, [{callback,
    fun(_, _) -> GUI_PID ! stop end}]),
  wxButton:connect(LightButton, command_button_clicked, [{callback,
    fun(_, _) ->
      State = data_manager:lookup(process_orchestrator:processes_set(), light_state),
      if State == on ->
        sensor_controller_listener_PID() ! {light_swtich, off};
        true -> sensor_controller_listener_PID() ! {light_swtich, on}
      end
    end}]),
  wxFrame:show(Frame),

  await_start(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText).

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
  wxStaticText:new(Panel, 16, "Phone:", [{pos, {100, 230}}, {size, {130, 25}}]).

await_start(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText) ->
  receive
    start -> app_warmup:initiate_app();
    close -> wx:destroy(), process_orchestrator:close_app()
  end,
  await_command(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText).

await_command(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText) ->
  receive
    {blind_receiver_listener1, blindDown} -> wxStaticText:setLabel(BlindsText1, "Down");

    {blind_receiver_listener1, blindUp} -> wxStaticText:setLabel(BlindsText1, "Up");

    {blind_receiver_listener2, blindDown} -> wxStaticText:setLabel(BlindsText2, "Down");

    {blind_receiver_listener2, blindUp} -> wxStaticText:setLabel(BlindsText2, "Up");

    {temp, Temp} -> wxStaticText:setLabel(TempText, io_lib:format("~.1f °C", [Temp]));

    {phone, Notification} -> wxStaticText:setLabel(PhoneText, Notification);

    {climateOn} -> wxStaticText:setLabel(ClimateText, "On");

    {climateOff} -> wxStaticText:setLabel(ClimateText, "Off");

    {outletOn} -> wxStaticText:setLabel(OutletText, "On");

    {outletOff} -> wxStaticText:setLabel(OutletText, "Off");

    {smokeOn} -> wxStaticText:setLabel(SmokeText, "Yes");

    {smokeOff} -> wxStaticText:setLabel(SmokeText, "No");

    stop -> app_warmup:terminate_app(), ets:insert(process_orchestrator:processes_set(), {guiPID, self()});

    start -> app_warmup:initiate_app();

    close -> wx:destroy(), process_orchestrator:close_app();

    Command -> io:format("Unknown signal ~p~n", [Command])
  end,
  await_command(BlindsText1, BlindsText2, ClimateText, OutletText, SmokeText, TempText, PhoneText).