%% -------------------------------------------------------------------
%% @author Sirius Zhuang <siriusibunny@gmail.com>
%%   [http://github.com/siriuszhuang]
%% @version 0.0.1
%% @doc This is a module for analysor data from client. Default
%% analysor for gg_framework. The api action defined in file
%% include/config/api_action.hrl.
%% @end
%% -------------------------------------------------------------------
-module(gg_analysor).

-export([
  analysis/3,
  unpack/3
]).

-include_lib("include/config/api_action.hrl").

%% -------------------------------------------------------------------
%% @doc Analysis the data from client and decode the binary data to
%% erlang variable.
%% @end
%% -------------------------------------------------------------------
-spec analysis(
  CallCode::integer(),
  ErrorCode::integer(),
  RawData::binary()
) -> {ok, Data::tuple()} | {error, Reason::atom()}.
analysis(CallCode, ErrorCode, RawData) ->
  case find_action(CallCode) of
    [ActionRoute | []] ->
      [
        {controller, Controller},
        {action, Action},
        {params, ParamsRoute}
      ] = ActionRoute,
      {params, Params} = unpack(ParamsRoute, ErrorCode, RawData),
      {ok, {
        {controller, Controller},
        {action, Action},
        {params, Params}
      }};
    _ ->
      {error, route_error}
  end.

find_action(CallCode) ->
  {api, ApiRoute} = route(),
  [Action || [Code | Action] <- ApiRoute, CallCode == Code].

unpack(ParamsRoute, _ErrorCode, RawData) ->
  {Params, _RawDataLeft} = unpack_params(RawData, ParamsRoute),
  {params, Params}.

unpack_params(RawData, [HeadParams | TailParams]) ->
  case HeadParams of
    {_ParamName, ParamType} ->
      {Data, RawDataLeft} = decode_binary(RawData, ParamType),
      case  unpack_params(RawDataLeft, TailParams) of
        {UnpackData, RawDataLastLeft} ->
          {[Data | UnpackData], RawDataLastLeft}
      end;
    {_ParamName, list, SubParams} ->
      <<
        ListCount:16/unsigned-big-integer,
        RawDataLeft/binary
      >> = RawData,
      unpack_list(ListCount, RawDataLeft, SubParams, [])
  end;
unpack_params(RawData, []) ->
  {[], RawData}.

unpack_list(0, RawData, _ParamsRoute, List) ->
  {[lists:reverse(List)], RawData};
unpack_list(LeftCount, RawData, ParamsRoute, List) ->
  {
    UnpackData,
    RawDataLeft
  } = unpack_params(RawData, ParamsRoute),
  io:format("UnpackData: ~p~n", [UnpackData]),
  unpack_list(
    LeftCount - 1,
    RawDataLeft,
    ParamsRoute,
    [UnpackData | List]
  ).

decode_binary(RawData, DataType) ->
  case DataType of
    string ->
      <<
        DataLength:16/unsigned-big-integer,
        BinData/binary
      >> = RawData,
      {
        Data,
        RawDataLeft
      } = split_binary(BinData, DataLength),
      {
        binary_to_list(Data),
        RawDataLeft
      };
    integer ->
      case RawData of
        <<Data:32, RawDataLeft/binary>> ->
          {Data, RawDataLeft};
        <<Data:32>> ->
          {Data, <<>>}
      end
  end.
