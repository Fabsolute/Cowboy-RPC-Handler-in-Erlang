-module(rpc_ws_handler).
%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-callback init(State :: term()) ->
  {ok, State :: term()}.

-callback info(Info :: term(), State :: term()) ->
  {ok, State :: term()}.

-callback terminate(Reason :: term(), Request :: term(), State :: term()) ->
  ok.

-optional_callbacks([init/1, info/2, terminate/3]).

-record(state, {
  handler_module,
  decoder_module,
  decoder_function,
  encoder_module,
  encoder_function,
  sub_state
}).

%%%===================================================================
%%% websocket callbacks
%%%===================================================================

init(Request, Args) ->
  Path = binary_to_list(cowboy_req:path(Request)),
  #{route_path:=RoutePath} = Args,
  Sub = string:sub_string(Path, length(RoutePath) + 1),
  case Sub of
    "/js" ->
      handle_js_generate(Request, Args);
    _ ->
      handle_socket(Request, Args)
  end.

handle_js_generate(Request, Args) ->
  #{server_handler:=ServerHandlerModule, client_handler:=ClientHandlerModule} = Args,
  Method = cowboy_req:method(Request),
  Request2 = js_generator:handle(Method, Request, ClientHandlerModule, ServerHandlerModule),
  {ok, Request2, Args}.

handle_socket(Request, Args) ->
  #{server_handler:=ServerHandler, decoder:={DecoderModule, DecoderFunction}, encoder:={EncoderModule, EncoderFunction}, state:=SubState} = Args,
  Opts = #{compress => true, idle_timeout => 300000},
  State = #state{
    handler_module = ServerHandler,
    decoder_module = DecoderModule,
    decoder_function = DecoderFunction,
    encoder_module = EncoderModule,
    encoder_function = EncoderFunction,
    sub_state = SubState
  },
  {cowboy_websocket, Request, State, Opts}.
websocket_init(#state{handler_module = HandlerModule, sub_state = SubState} = State) ->
  case erlang:function_exported(HandlerModule, init, 1) of
    true ->
      {ok, NewSubState} = HandlerModule:init(SubState),
      {ok, State#state{sub_state = NewSubState}};
    _ ->
      {ok, State}
  end.

websocket_handle(Data, #state{decoder_module = DecoderModule, decoder_function = DecoderFunction} = State) ->
  case DecoderModule:DecoderFunction(Data) of
    {ok, Message} ->
      handle(Message, State);
    _ ->
      bad_request(State)
  end.

websocket_info({rpc, _Method, FunctionId, Parameters}, #state{sub_state = SubState} = State) ->
  Content = #{"m"=>FunctionId, "p"=>Parameters},
  Response = {reply, Content, SubState},
  handle_response(Response, State);
websocket_info(Info, #state{handler_module = HandlerModule, sub_state = SubState} = State) ->
  case erlang:function_exported(HandlerModule, info, 2) of
    true ->
      handle_response(HandlerModule:info(Info, SubState), State);
    _ ->
      {ok, State}
  end.

terminate(Reason, Request, #state{handler_module = HandlerModule, sub_state = SubState} = _State) ->
  case erlang:function_exported(HandlerModule, terminate, 3) of
    true ->
      HandlerModule:terminate(Reason, Request, SubState);
    _ ->
      ok
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle(#{"m":=FunctionId} = Request, State) when is_float(FunctionId) ->
  handle(Request#{"m"=>trunc(FunctionId)}, State);
handle(#{"m":=FunctionId, "p" := Parameters}, #state{handler_module = HandlerModule, sub_state = SubState} = State) ->
  Functions = HandlerModule:exported_rpc_functions(),
  MethodExist = lists:any(
    fun({_MethodName, Id, _Parameters}) -> Id == FunctionId end,
    Functions
  ),
  case MethodExist of
    true ->
      {Method, FunctionId, _Parameters} = lists:keyfind(FunctionId, 2, Functions),
      handle_response(HandlerModule:Method(list_to_tuple(Parameters), SubState), State);
    _ ->
      not_found(State)
  end;

handle(_, State) ->
  bad_request(State).

handle_response(Response, #state{encoder_module = EncoderModule, encoder_function = EncoderFunction} = State) ->
  case Response of
    {ok, NewSubState} ->
      {ok, State#state{sub_state = NewSubState}};
    {reply, SubResponse, NewSubState} ->
      {ok, EncodedResponse} = EncoderModule:EncoderFunction(SubResponse),
      {reply, {binary, EncodedResponse}, State#state{sub_state = NewSubState}}
  end.

bad_request(State) ->
  % todo reply bad_request
  {ok, State}.

not_found(State) ->
  % todo reply not_found
  {ok, State}.
