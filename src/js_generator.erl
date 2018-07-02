-module(js_generator).

-export([handle/4]).

handle(<<"GET">>, Request, ClientHandlerModule, ServerHandlerModule) ->
  Response = generate_javascript(ClientHandlerModule, ServerHandlerModule),
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain; charset=utf-8">>
  }, Response, Request);
handle(_, Req, _ClientHandlerModule, _ServerHandlerModule) ->
  cowboy_req:reply(405, Req).

generate_javascript(ClientHandlerModule, ServerHandlerModule) ->
  ClientFunctions = ClientHandlerModule:exported_rpc_functions(),
  ServerFunctions = ServerHandlerModule:exported_rpc_functions(),
  ClientResponse = "client_functions:" ++ generate_client_functions(ClientFunctions),
  ServerResponse = "server:{" ++ generate_server_functions(ServerFunctions) ++ "}",

  "SuperSocket.prototype.getFunctions = function() { const $this = this; return {" ++ ClientResponse ++ ServerResponse ++ "};};".

generate_client_functions(ClientFunctions) ->
  generate_client_functions(ClientFunctions, []).
generate_client_functions([], Acc) ->
  "{" ++ lists:join(",", Acc) ++ "},";
generate_client_functions([{Name, Id, Parameters} | T], Acc) ->
  generate_client_functions(T, Acc ++ [make_javascript_object(Id, Name, Parameters)]).

generate_server_functions(ServerFunctions) ->
  generate_server_functions(ServerFunctions, []).
generate_server_functions([], Acc) ->
  lists:join(",", Acc);
generate_server_functions([{Name, Id, Parameters} | T], Acc) ->
  generate_server_functions(T, Acc ++ [make_javascript_function(Id, Name, Parameters)]).

make_javascript_function(Id, Name, Parameters) ->
  NameString = atom_to_list(Name),
  ParametersString = erlang_to_javascript_parameters(Parameters),
  NameString ++ ":(" ++ ParametersString ++ ")=>{$this.execute(" ++ integer_to_list(Id) ++ ", [" ++ ParametersString ++ "]);}".

make_javascript_object(Id, Name, Parameters) ->
  NameString = atom_to_list(Name),
  integer_to_list(Id) ++ ":{name:'" ++ NameString ++ "', params:'" ++ erlang_to_javascript_parameters(Parameters) ++ "'}".

erlang_to_javascript_parameters(Parameters) ->
  erlang_to_javascript_parameters(Parameters, []).
erlang_to_javascript_parameters([], Acc) ->
  lists:join(",", Acc);
erlang_to_javascript_parameters([P | T], Acc) ->
  erlang_to_javascript_parameters(T, Acc ++ ["" ++ atom_to_list(P) ++ ""]).
