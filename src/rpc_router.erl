-module(rpc_router).

%% API
-export([compile/1]).

compile(Routes) ->
  compile(Routes, []).

compile([], Acc) ->
  Acc;
compile([H | T], Acc) ->
  #{
    route_path := RoutePath,
    server_handler := _ServerHandler,
    client_handler := _ClientHandler,
    decoder := {_Decoder, _DecoderFunc},
    encoder := {_Encoder, _EncoderFunc},
    state := _State
  } = H,
  compile(T, Acc ++ [{RoutePath ++ "/[...]", rpc_ws_handler, H}]).
