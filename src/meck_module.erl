%%%-------------------------------------------------------------------
%%% File:      meck_module.erl
%%% @author    cliff moon <> []
%%% @copyright 2011 cliff moon
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2011-03-05 by cliff moon
%%%-------------------------------------------------------------------
-module(meck_module).
-author('').

%% API
-export([backup_original/1, compile_forms/1, cleanup/1, 
         restore_original/2, exists/1, exports/1,
         original_name/1, unload_if_mocked/4]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------

backup_original(Module) ->
    Cover = get_cover_state(Module),
    try
        Forms = abstract_code(beam_file(Module)),
        NewName = original_name(Module),
        compile_forms(rename_module(Forms, NewName), compile_options(Module))
    catch
        throw:{object_code_not_found, _Module} -> ok; % TODO: What to do here?
        throw:no_abstract_code                 -> ok  % TODO: What to do here?
    end,
    Cover.
    
restore_original(_Mod, false) ->
    ok;
restore_original(Mod, {File, Data, Options}) ->
    case filename:extension(File) of
        ".erl" ->
            {ok, Mod} = cover:compile_module(File, Options);
        ".beam" ->
            cover:compile_beam(File)
    end,
    ok = cover:import(Data),
    file:delete(Data),
    ok.

exists(Module) ->
    code:which(Module) /= non_existing.

exports(Module) ->
    [ FA ||  FA  <- Module:module_info(exports),
             FA /= {module_info, 0}, FA /= {module_info, 1}].

compile_forms(AbsCode) -> compile_forms(AbsCode, []).

compile_forms(AbsCode, Opts) ->
    case compile:forms(AbsCode, Opts) of
        {ok, ModName, Binary} ->
            load_binary(ModName, Binary);
        {ok, ModName, Binary, _Warnings} ->
            load_binary(ModName, Binary)
    end.
    
cleanup(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(original_name(Mod)),
    code:delete(original_name(Mod)).

original_name(Name) -> list_to_atom(atom_to_list(Name) ++ "_meck_original").

%%====================================================================
%% Internal functions                                                 
%%====================================================================
get_cover_state(Module) -> get_cover_state(Module, cover:is_compiled(Module)).

get_cover_state(Module, {file, File}) ->
    Data = atom_to_list(Module) ++ ".coverdata",
    ok = cover:export(Data, Module),
    CompileOptions =
        try
            compile_options(beam_file(Module))
        catch
            throw:{object_code_not_found, _Module} -> []
        end,
    {File, Data, CompileOptions};
get_cover_state(_Module, _IsCompiled) ->
    false.

load_binary(Name, Binary) ->
    case code:load_binary(Name, "", Binary) of
        {module, Name}  -> ok;
        {error, Reason} -> exit({error_loading_module, Name, Reason})
    end.

beam_file(Module) ->
    % code:which/1 cannot be used for cover_compiled modules
    case code:get_object_code(Module) of
        {_, Binary, _Filename} -> Binary;
        error                  -> throw({object_code_not_found, Module})
    end.

abstract_code(BeamFile) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {_, [{abstract_code, no_abstract_code}]}} ->
            throw(no_abstract_code)
    end.

compile_options(BeamFile) when is_binary(BeamFile) ->
    case beam_lib:chunks(BeamFile, [compile_info]) of
        {ok, {_, [{compile_info, Info}]}} ->
            proplists:get_value(options, Info);
        _ ->
            []
    end;
compile_options(Module) ->
    proplists:get_value(options, Module:module_info(compile)).

rename_module([{attribute, Line, module, _OldName}|T], NewName) ->
    [{attribute, Line, module, NewName}|T];
rename_module([H|T], NewName) ->
    [H|rename_module(T, NewName)].

unload_if_mocked(P, L, Suffix, Unload) when is_atom(P) ->
    unload_if_mocked(atom_to_list(P), L, Suffix, Unload);
unload_if_mocked(P, L, Suffix, Unload) when length(P) > length(Suffix) ->
    case lists:split(length(P) - length(Suffix), P) of
        {Name, Suffix} ->
            Mocked = list_to_existing_atom(Name),
            Unload(Mocked),
            [Mocked|L];
        _Else ->
            L
    end;
unload_if_mocked(_P, L, _, _) ->
    L.