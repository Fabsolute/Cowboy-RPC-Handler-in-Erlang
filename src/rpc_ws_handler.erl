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
      try HandlerModule:init(SubState) of
        {ok, NewSubState} ->
          {ok, State#state{sub_state = NewSubState}}
      catch
        Error ->
          on_exception(-1, Error, State)
      end;
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
  Content = [false, FunctionId, Parameters],
  Response = {reply, Content, SubState},
  handle_response(Response, State);
websocket_info(Info, #state{handler_module = HandlerModule, sub_state = SubState} = State) ->
  case erlang:function_exported(HandlerModule, info, 2) of
    true ->
      try HandlerModule:info(Info, SubState) of
        Response ->
          handle_response(Response, State)
      catch
        Error ->
          on_exception(-1, Error, State)
      end;
    _ ->
      {ok, State}
  end.

terminate(Reason, Request, #state{handler_module = HandlerModule, sub_state = SubState} = State) ->
  case erlang:function_exported(HandlerModule, terminate, 3) of
    true ->
      try HandlerModule:terminate(Reason, Request, SubState) of
        ok ->
          ok
      catch
        Error ->
          on_exception(-1, Error, State)
      end;
    _ ->
      ok
  end;
terminate(_Reason, _Request, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle([FunctionId | Rest], State) when is_float(FunctionId) ->
  handle([trunc(FunctionId) | Rest], State);
handle([FunctionId, MessageId, Parameters], State) when is_float(MessageId) ->
  handle([FunctionId, trunc(MessageId), Parameters], State);
handle([FunctionId, MessageId, Parameters], #state{handler_module = HandlerModule, sub_state = SubState} = State) ->
  Functions = HandlerModule:exported_rpc_functions(),
  MethodExist = lists:any(
    fun({_MethodName, Id, _Parameters}) -> Id == FunctionId end,
    Functions
  ),
  case MethodExist of
    true ->
      {Method, FunctionId, _Parameters} = lists:keyfind(FunctionId, 2, Functions),
      try HandlerModule:Method(list_to_tuple(Parameters), SubState) of
        Response ->
          handle_response(Response, MessageId, State)
      catch
        Error ->
          on_exception(MessageId, Error, State)
      end;
    _ ->
      not_found(State)
  end;

handle(_, State) ->
  bad_request(State).
handle_response(Response, State) ->
  handle_response(Response, -1, State).

handle_response(Response, MessageId, State) ->
  case Response of
    {ok, NewSubState} ->
      case MessageId of
        -1 ->
          {ok, State#state{sub_state = NewSubState}};
        _ ->
          send_reply(MessageId, undefined, State#state{sub_state = NewSubState})
      end;
    {reply, SubResponse, NewSubState} ->
      send_reply(MessageId, SubResponse, State#state{sub_state = NewSubState})
  end.

bad_request(State) ->
  % todo reply bad_request
  {ok, State}.

not_found(State) ->
  % todo reply not_found
  {ok, State}.

on_exception(MessageId, Error, #state{handler_module = HandlerModule, sub_state = SubState} = State) ->
  case erlang:function_exported(HandlerModule, on_exception, 2) of
    true ->
      case HandlerModule:on_exception(Error, SubState) of
        {ok, NewSubState} ->
          case MessageId of
            -1 ->
              {ok, State#state{sub_state = NewSubState}};
            _ ->
              send_reply(MessageId, undefined, State#state{sub_state = NewSubState})
          end;
        {reply, Response, NewSubState} ->
          case MessageId of
            -1 ->
              {ok, State#state{sub_state = NewSubState}};
            _ ->
              send_reply(MessageId, Response, State#state{sub_state = NewSubState})
          end
      end;
    _ ->
      {ok, State}
  end.

send_reply(MessageId, Response, #state{encoder_module = EncoderModule, encoder_function = EncoderFunction} = State) ->
  Message =
    case Response of
      undefined ->
        [true, MessageId];
      [false, FunctionId, Parameters] ->
        [false, FunctionId, Parameters];
      _ ->
        [true, MessageId, Response]
    end,
  {ok, EncodedResponse} = EncoderModule:EncoderFunction(Message),
  {reply, {binary, EncodedResponse}, State}.
