-module(rpc_socket).

%% API
-export([parse_transform/2]).

parse_transform(Form, _Opts) ->
  TypeAndFunctions = get_rpc_type_and_functions(Form),
  case TypeAndFunctions of
    false ->
      Form;
    {Type, RPCFunctions} ->
      case length(RPCFunctions) of
        0 ->
          Form;
        _ ->
          case Type of
            client ->
              ParsedFunctions = parse_rpc_functions(Form, RPCFunctions, [], client),
              inject_client_functions(Form, ParsedFunctions);
            server ->
              ParsedFunctions = parse_rpc_functions(Form, RPCFunctions, [], server),
              inject_server_rpc_functions(Form, ParsedFunctions);
            _ ->
              Form % todo type error
          end
      end
  end.

inject_client_functions(Form, RPCFunctions) ->
  inject(
    fun(Injector, List, Acc) ->
      case List of
        [{eof, Line} | T] ->
          inject(Injector, T, Acc ++ [rpc_list_to_function(RPCFunctions, Line), {eof, Line + 1}]);
        [{function, Line, FunctionName, Arity, Function} = H | T] ->
          MatchedFunction = lists:keyfind(FunctionName, 1, RPCFunctions),
          case MatchedFunction of
            false ->
              inject(Injector, T, Acc ++ [H]);
            {FunctionName, FunctionId, Parameters} ->
              case length(Parameters) + 1 of
                Arity ->
                  case Function of
                    [{clause, ClauseLine, [_H | Params], Guards, [{atom, AtomLine, ok}]}] ->
                      MyFunction = {
                        function,
                        Line,
                        FunctionName,
                        Arity,
                        [
                          {clause, ClauseLine, [{var, AtomLine, 'Pid'} | Params], Guards,
                            [{op, AtomLine,
                              '!',
                              {var, 33, 'Pid'},
                              {tuple, AtomLine, [
                                {atom, AtomLine, rpc},
                                {atom, AtomLine, FunctionName},
                                {integer, AtomLine, FunctionId},
                                parameters_to_function_content(Parameters, AtomLine)
                              ]}
                            }]
                          }]},
                      inject(Injector, T, Acc ++ [MyFunction]);
                    _ ->
                      inject(Injector, T, Acc ++ [H]) % todo injection error
                  end;
                _ ->
                  inject(Injector, T, Acc ++ [H]) % todo injection error
              end
          end;
        [H | T] ->
          inject(Injector, T, Acc ++ [H])
      end
    end,
    Form,
    []
  ).

inject_server_rpc_functions(Form, RPCFunctions) ->
  inject(
    fun(FunctionMaker, List, Acc) ->
      case List of
        [{eof, Line} | T] ->
          inject(FunctionMaker, T, Acc ++ [rpc_list_to_function(RPCFunctions, Line), {eof, Line + 1}]);
        [H | T] ->
          inject(FunctionMaker, T, Acc ++ [H])
      end
    end,
    Form,
    []
  ).

inject(_Injector, [], Acc) ->
  Acc;
inject(Injector, [{attribute, _Line, export, ExportList} | T], Acc) ->
  inject(Injector, T, Acc ++ [{attribute, _Line, export, [{exported_rpc_functions, 0} | ExportList]}]);
inject(Injector, [H | T], Acc) ->
  Injector(Injector, [H | T], Acc).

%%get_file([{attribute, _, file, {File, _}} | _]) -> File;
%%get_file([_ | T]) -> get_file(T).

get_rpc_type_and_functions([]) ->
  false;
get_rpc_type_and_functions([{attribute, _Line, rpc, {Type, RPCFunctionList}} | _T]) when is_list(RPCFunctionList) ->
  {Type, RPCFunctionList};
get_rpc_type_and_functions([_H | T]) ->
  get_rpc_type_and_functions(T).

parse_rpc_functions([], _RPCFunctions, Acc, _Type) ->
  Acc;
parse_rpc_functions([{function, _Line, FunctionName, Arity, Function} | T], RPCFunctions, Acc, Type) ->
  case Arity of
    2 ->
      do_parse_rpc_functions(T, FunctionName, Function, RPCFunctions, Acc, Type);
    _ ->
      parse_rpc_functions(T, RPCFunctions, Acc, Type)
  end;
parse_rpc_functions([_H | T], RPCFunctions, Acc, Type) ->
  parse_rpc_functions(T, RPCFunctions, Acc, Type).


do_parse_rpc_functions(T, FunctionName, Function, RPCFunctions, Acc, Type) ->
  IsRPCFunction = lists:any(fun(RPCName) -> FunctionName =:= RPCName end, RPCFunctions),
  case IsRPCFunction of
    false ->
      parse_rpc_functions(T, RPCFunctions, Acc, Type);
    true ->
      case Function of
        [{clause, _, Params, _, _}] ->
          ParameterList =
            case Params of
              [{tuple, _, ServerParams} | _] ->
                case Type of
                  server ->
                    ServerParams;
                  _ ->
                    []
                end;
              [_ | {tuple, _, ClientParams}] ->
                case Type of
                  client ->
                    ClientParams;
                  _ ->
                    []
                end;
              _ ->
                []
            end,
          Parameters = parse_rpc_function_parameters(ParameterList, []),
          case Parameters of
            false ->
              parse_rpc_functions(T, RPCFunctions--[FunctionName], Acc, Type);
            _ ->
              parse_rpc_functions(
                T,
                RPCFunctions--[FunctionName],
                Acc ++ [{FunctionName, rand:uniform(16#ffffffff), Parameters}],
                Type
              )
          end;
        _ ->
          parse_rpc_functions(T, RPCFunctions--[FunctionName], Acc, Type)
      end
  end.

parse_rpc_function_parameters([], Acc) ->
  Acc;
parse_rpc_function_parameters([{var, _Line, Name} | T], Acc) ->
  parse_rpc_function_parameters(T, Acc ++ [Name]);
parse_rpc_function_parameters([_ | _T], _Acc) ->
  false.

rpc_list_to_function(List, Line) ->
  {function, Line, exported_rpc_functions, 0,
    [{clause, Line, [], [],
      [rpc_list_to_function_content(List, Line)]}]}.

rpc_list_to_function_content([], Line) ->
  {nil, Line};
rpc_list_to_function_content([{Name, Id, Parameters} | T], Line) ->
  Content = {
    tuple,
    Line,
    [{atom, Line, Name}, {integer, Line, Id}, atom_list_to_function_content(Parameters, Line)]
  },
  {cons, Line, Content, rpc_list_to_function_content(T, Line)}.

atom_list_to_function_content(List, Line) ->
  type_list_to_function_content(List, Line, atom).

type_list_to_function_content([], Line, _Type) ->
  {nil, Line};
type_list_to_function_content([Name | T], Line, Type) ->
  Content = {Type, Line, Name},
  {cons, Line, Content, type_list_to_function_content(T, Line, Type)}.


parameters_to_function_content(List, Line) ->
  type_list_to_function_content(List, Line, var).
