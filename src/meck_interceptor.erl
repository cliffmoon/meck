%%%-------------------------------------------------------------------
%%% File:      meck_interceptor.erl
%%% @author    cliff moon <cliff@fastip.com> []
%%% @copyright 2011 cliff moon
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2011-03-04 by cliff moon
%%%-------------------------------------------------------------------
-module(meck_interceptor).
-author('cliff@fastip.com').

%% API
-export([new/2]).

-record(context, {module,transforms}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------
new(Module, Xforms) ->
  {Module, Bin, _} = code:get_object_code(Module),
  {ok, {Module,[{abstract_code,{raw_abstract_v1,Forms}}]}} = beam_lib:chunks(Bin, [abstract_code]),
  file:write_file("original.erl", io_lib:format("~p~n", [Forms])),
  intercept_forms(#context{module=Module,transforms=Xforms}, Forms).
%%====================================================================
%% Internal functions
%%====================================================================
intercept_forms(Ctx,Forms) ->
  intercept_forms(Ctx,Forms,[]).
  
intercept_forms(_Ctx,[],Acc) ->
  lists:reverse(Acc);
intercept_forms(Ctx,[F|Forms],Acc) when not is_tuple(F) ->
  intercept_forms(Ctx,Forms,[F|Acc]);
intercept_forms(Ctx,[Form|Forms],Acc) ->
  Forms2 = xform(Ctx, Form),
  FormList = lists:map(fun(Element) ->
      if
        is_list(Element) -> intercept_forms(Ctx,Element);
        is_tuple(Element) -> 
          [E] = intercept_forms(Ctx,[Element]),
          E;
        true -> Element
      end
    end, tuple_to_list(Forms2)),
  Form2 = list_to_tuple(FormList),
  intercept_forms(Ctx,Forms,[Form2|Acc]).
  
xform(#context{transforms=Xforms}, Form) ->
  Tag = element(1, Form),
  case proplists:get_value(Tag, Xforms) of
    undefined -> Form;
    Xform -> Xform(Form)
  end.

% intercept_functions(Ctx, [], Acc) ->
%   lists:reverse(Acc);
% intercept_functions(Ctx, [F={function,Line,Name,Arity,Clauses}|Functions], Acc) ->
%   
%   Clauses2 = intercept_clauses(Ctx, Clauses),
%   intercept_functions(Ctx, Functions, [{function,Line,Name,Arity,Clauses2}|Acc]);
% intercept_functions(Ctx, [Form|Forms], Acc) ->
%   intercept_functions(Ctx, Forms, [Form|Acc]).
%   
% intercept_clauses(Ctx, Clauses) ->
%   intercept_clauses(Ctx, Clauses, []).
%   
% intercept_clauses(Ctx, [], Acc) ->
%   lists:reverse(Acc);
% intercept_clauses(Ctx, [{clause,Line,Vars,Guard,Expressions}|Clauses], Acc) ->
%   Expressions2 = intercept_expressions(Ctx, Expressions),
%   intercept_clauses(Ctx, Clauses, [{clause,Line,Vars,Guard,Expressions2}|Acc]).
%   
% intercept_expressions(Ctx, Expressions) ->
%   intercept_expressions(Ctx, Expressions, []).
%   
% intercept_expressions(Ctx, [], Acc) ->
%   lists:reverse(Acc);
% intercept_expressions(Ctx, [{match,Line,Pattern,Body}|Expressions], Acc) ->
%   [Pattern2] = intercept_expressions(Ctx, [Pattern]),
%   [Body2] = intercept_expressions(Ctx, [Body]),
%   intercept_expressions(Ctx, Expressions, [{match,Line,Pattern,Body}|Expressions]);
% intercept_expressions(Ctx, [{var,Line,Name}|Expressions], Acc) ->
%   intercept_expressions(Ctx, Expressions, [{var,Line,Name}|Acc]);
% intercept_expressions(Ctx, [{tuple,Line,Elements}|Expressions], Acc) ->
%   Elements2 = intercept_expressions(Ctx, Elements),
%   intercept_expressions(Ctx, Expressions, [{tuple,Line,Elements2}|Acc]);
% intercept_expressions(Ctx, [{nil,Line}|Expressions], Acc) ->
%   intercept_expressions(Ctx, Expressions, [{nil,Line}|Acc]);
% intercept_expressions(Ctx, [{cons,Line,Head,Tail}|Expressions], Acc) ->
%   Head2 = intercept_expressions(Ctx,Head),
%   Tail2 = intercept_expressions(Ctx,Tail),
%   intercept_expressions(Ctx, Expressions, [{cons,Line,Head,Tail}|Acc]);
% intercept_expressions(Ctx, [{bin,Line,Fields}|Expressions], Acc) ->
%   Fields2 = intercept_binary_fields(Ctx, Fields),
%   intercept_expressions(Ctx, Expressions, [{bin,Line,Fields2}|Acc]);
% intercept_expressions(Ctx, [{op,Line,Op,Left,Right}|Expressions], Acc) ->
%   [Left2] = intercept_expressions(Ctx, [Left]),
%   [Right2] = intercept_expressions(Ctx, [Right]),
%   intercept_expressions(Ctx, Expressions, [{op,Line,Op,Left2,Right2}|Acc]);
% intercept_expressions(Ctx, [{op,Line,Op,Arg}|Expressions], Acc) ->
%   [Arg2] = intercept_expressions(Ctx, [Arg]),
%   intercept_expressions(Ctx, Expressions, [{op,Line,Op,Arg2}|Acc]);
% intercept_expressions(Ctx, [{record,Line,Type,Fields}|Expressions], Acc) ->
%   Fields2 = intercept_fields(Ctx, Fields),
%   intercept_expressions(Ctx, Expressions, [{record,Line,Type,Fields2}|Acc]);
% intercept_expressions(Ctx, [{record,Line,Argument,Type,Fields}|Expressions], Acc) ->
%   [Argument2] = intercept_expressions(Ctx, [Argument]),
%   Fields2 = intercept_fields(Ctx, Fields),
%   intercept_expressions(Ctx, Expressions, [{record,Line,Argument2,Type,Fields2}|Acc]);
% intercept_expressions(Ctx, [{record_index,Line,Type,Field}|Expressions], Acc) ->
%   [Field2] = intercept_fields(Ctx, [Field]),
%   intercept_expressions(Ctx, Expressions, [{record_index,Line,Type,Field2}|Acc]);
% intercept_expressions(Ctx, [{'catch',Line,Exp}|Expressions], Acc) ->
%   [Exp2] = intercept_expressions(Ctx, [Exp]),
%   intercept_expressions(Ctx, Expressions, [{'catch',Line,Exp2}|Acc]);
% intercept_expressions(Ctx, [{call,Line,Fun,Args}|Expressions], Acc) ->
%   [Fun2] = intercept_expressions(Ctx, [Fun]),
%   Args2 = intercept_expressions(Ctx, Args),
%   intercept_expressions(Ctx, Expressions, [{call,Line,Fun2,Args2}|Acc]);
% intercept_expressions(Ctx, [{'case',Line,Exp,Clauses}|Expressions], Acc) ->
%   [Exp2] = intercept_expressions(Ctx, [Exp]),
%   Clauses2 = intercept_clauses(Ctx, Clauses),
%   intercept_expressions(Ctx, Expressions, [{call,Line,Exp2,Clauses2}|Acc]);
% intercept_expressions(Ctx, [{'try',Line,Body,Clauses,Handlers,After}|Expressions], Acc) ->
%   Body2 = intercept_expressions(Ctx, Body),
%   Clauses2 = intercept_clauses(Ctx, Clauses),
%   Handlers2 = intercept_clauses(Ctx, Handlers),
%   After2 = intercept_expressions(Ctx, After),
%   intercept_expressions(Ctx, Expressions, [{'try',Line,Body2,Clauses2,Handlers2,After2}|Acc]);
% intercept_expressions(Ctx, [{'if',Line,Clauses}|Expressions], Acc) ->
%   Clauses2 = intercept_clauses(Ctx, Clauses),
%   intercept_expressions(Ctx, Expressions, [{'if',Line,Clauses2}|Acc]);
% intercept_expressions(Ctx, [{lc,Line,Template,Body}|Expressions], Acc) ->
%   [Template2] = intercept_expressions(Ctx, [Template]),
%   Body2 = intercept_expressions(Ctx, Body),
%   intercept_expressions(Ctx, Expressions, [{lc,Line,Template2,Body2}|Acc]);
% intercept_expressions(Ctx, [{'receive',Line,Clauses}|Expressions], Acc) ->
%   Clauses2 = intercept_clauses(Ctx, Clauses),
%   intercept_expressions(Ctx, Expressions, [{'receive',Line,Clauses2}|Acc]);
% intercept_expressions(Ctx, [{'receive',Line,Clauses,Timeout,Action}|Expressions], Acc) ->
%   Clauses2 = intercept_clauses(Ctx, Clauses),
%   [Timeout2] = intercept_expressions(Ctx, [Timeout]),
%   Action2 = intercept_expressions(Ctx, Action),
%   intercept_expressions(Ctx, Expressions, [{'receive',Line,Clauses2,Timeout2,Action2}|Acc]);
% intercept_expressions(Ctx, [{'fun',Line,{function,Name,Arity}}|Expressions], Acc) ->
%   intercept_expressions(Ctx, Expressions, [{'fun',Line,{function,Name,Arity}}|Acc]);
% intercept_expressions(Ctx, [{'fun',Line,{function,Ctx,Name,Arity}}|Expressions], Acc) ->
%   intercept_expressions(Ctx, Expressions, [{'fun',Line,{function,Ctx,Name,Arity}}]);
% intercept_expressions(Ctx, [{'fun',Line,{clauses,Clauses}}|Expressions], Acc) ->
%   Clauses2 = intercept_clauses(Ctx, Clauses),
%   intercept_expressions(Ctx, Expressions, [{'fun',Line,{clauses,Clauses2}}|Acc]);
% intercept_expressions(Ctx, [{'query',Line,Body}|Expressions], Acc) ->
%   [Body2] = intercept_expressions(Ctx, [Body]),
%   intercept_expressions(Ctx, Expressions, [{'query',Line,Body2}|Acc]).
% 
% intercept_binary_fields(Ctx, Fields) ->
%   intercept_binary_fields(Ctx, Fields, []).
%   
% intercept_binary_fields(Ctx, [], Acc) ->
%   lists:reverse(Acc);
% intercept_binary_fields(Ctx, [{bin_element,Line,Exp,Size,TypeList}|Fields], Acc) ->
%   [Exp2] = intercept_expressions(Ctx, [Exp]),
%   [Size2] = intercept_expressions(Ctx, [Size]),
%   intercept_binary_fields(Ctx, Fields, [{bin_element,Line,Exp2,Size2,TypeList}|Acc]).
%   
% intercept_fields(Ctx, Fields) ->
%   intercept_fields(Ctx, Fields, []).
%   
% intercept_fields(Ctx, [], Acc) ->
%   lists:reverse(Acc);
% intercept_fields(Ctx, [{record_field,Line,Arg,Field}|Fields], Acc) ->
%   [Arg2] = intercept_expressions(Ctx, [Arg]),
%   [Field2] = intercept_expressions(Ctx, [Field]),
%   intercept_fields(Ctx, Fields, [{record_field,Line,Arg2,Field2}|Acc]);
% intercept_fields(Ctx, [{record_field,Line,Arg,Type,Field}|Fields], Acc) ->
%   [Arg2] = intercept_expressions(Ctx, [Arg]),
%   [Field2] = intercept_expressions(Ctx, [Field]),
%   intercept_fields(Ctx, Fields, [{record_field,Line,Arg2,Type,Field2}|Acc]).
% 
% apply_transforms(Form, Transforms) ->
%   Tag = element(1, Form),
%   case proplists:get_value(Tag, Transforms) of
%     undefined -> Form;
%     Xform -> Xform(Form)
%   end.
