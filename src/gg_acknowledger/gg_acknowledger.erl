%% -------------------------------------------------------------------
%% @author Sirius Zhuang <siriusibunny@gmail.com>
%%   [http://github.com/siriuszhuang]
%% @version 0.0.1
%% @doc This is a module for acknowledge to clients. Default
%% acknowledger for gg_framework. The acknowledge action defined in
%% include/config/ack_action.hrl.
%%
%% Usage:
%% File: include/config/ack_action.hrl
%% ```
%% ack_action() ->
%%   AckAction = [
%%     [100 | [
%%       {params, [
%%         {username, string},
%%         {Age, integer}
%%       ]}
%%     ]]
%%   ],
%%   {ok. AckAction}.
%% '''
%%
%% ```
%% AckCode = 100,
%% Username = "sirius@home.fun",
%% Age = 23,
%% gg_acknowledger:acknowledge({100, [Username, Age]}).
%% '''
%% @end
%% -------------------------------------------------------------------
-module(gg_acknowledger).

-export([
  acknowledge/1,
  pack/4,
  get_template/1
]).

-include_lib("include/config/ack_action.hrl").

%% -------------------------------------------------------------------
%% @doc Give an acknowledge code and a params list, params list must
%% match the structure defined in ack_action.hrl. It will encode to
%% binary data automatic. To return {ok, BinaryData} for success or
%% {error, Reason} for failed.
%% @end
%% -------------------------------------------------------------------
-spec acknowledge({AckCode::integer(), Params::list()}) ->
  {ok, binary()} | {error, atom()}.
acknowledge({AckCode, Params}) ->
  ErrorCode = 0,
  Res = case get_template(AckCode) of
    [ParamsTemplate | []] ->
      {params, TemplateList} = ParamsTemplate,
      pack(TemplateList, Params, AckCode, ErrorCode);
    _Other ->
      {error, ack_action}
  end,
  case Res of
    {error, Reason} -> {error, Reason};
    Data -> {ok, Data}
  end.

pack(TemplateList, Params, ActionCode, ErrorCode) ->
  ParamBinList = pack(TemplateList, Params),
  list_to_binary([<<ActionCode:16>>, <<ErrorCode:16>>, ParamBinList]).
  %<<ActionCode:16, ErrorCode:16, ParamBinList>>.

pack(TemplateList, Params) ->
  case length(Params) == length(TemplateList) of
    false -> {error, "Params count not match"};
    true  -> binary_data(TemplateList, Params)
  end.

binary_data(ParamsTemplate, Params) ->
  ParamBinList = pack_to_binary(ParamsTemplate, Params),
  io:format("$$$ ParamBinList: ~p~n", [ParamBinList]),
  %list_to_binary(ParamBinList).
  ParamBinList.

pack_to_binary([], []) ->
  <<>>;
pack_to_binary([Template | TTemplates], [Param | TParams]) ->
  case Template of
    {_ParamName, ParamType} ->
      io:format("$$$ 1: ~n"),
      [encode_to_binary(Param, ParamType) | pack_to_binary(TTemplates, TParams)];
    {_ParamName, list, SubParamsTemplate} ->
      io:format("$$$ 2: ~n"),
      ListCount = length(Param),
      io:format("$$$ ListCount: ~p~n", [ListCount]),
      [<<ListCount:16>> | pack_list(SubParamsTemplate, Param, ListCount)]
  end.

pack_list(_ParamsTemplate, _Params, 0) ->
  [];
pack_list(ParamsTemplate, [HParam | TParams], RestCount) ->
  [pack(ParamsTemplate, HParam) | pack_list(ParamsTemplate, TParams, RestCount - 1)].

encode_to_binary(Param, ParamType) ->
  io:format("$$$ encode_to_binary->Param: ~p~n", [Param]),
  case ParamType of
    string ->
      Length = length(Param),
      io:format("$$$ Length: ~p~n", [Length]),
      io:format("$$$ Param: ~p~n", [Param]),
      [<<Length:16>>, Param];
    integer ->
      <<Param:32>>;
    _Other ->
      throw({error, ?MODULE, {line, 60}})
  end.

get_template(AckCode) ->
  {ok, AckActionTemplate} = ack_action(),
  [Template || [Code, Template] <- AckActionTemplate, Code == AckCode].
