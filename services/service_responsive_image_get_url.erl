-module(service_responsive_image_get_url).
-author("Arthur Clemens").

-svc_title("Generate image data").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").

process_get(_ReqData, Context) ->
    Json = z_context:get_q("request", Context),
    Result = case Json of
        undefined -> {error, "no json data"};
        _ -> process_json_data(Json, Context)
    end,
    case Result of
        {ok, Url} -> {struct, [{url, Url}]};
        {error, Error} -> {struct, [{error, Error}]}
    end.

        
process_json_data(Json, Context) ->
    {struct, Data} = mochijson2:decode(Json),
    case check_required(Data) of
        {error, E} -> {error, E};
        ok ->
            MediaValue = proplists:get_value(<<"media_id">>, Data),
            MediaId = case string:to_integer(MediaValue) of
                {error, _Reason} -> MediaValue;            
                {IntegerValue, _Rest} -> IntegerValue
            end,
            HighResolution = z_convert:to_bool(proplists:get_value(<<"highResolution">>, Data, false)),
            
            PropKeys = [
                % integers
                <<"height">>,
                <<"quality">>,
                <<"width">>,
                % strings
                <<"background">>,
                <<"blur">>,
                <<"mediaclass">>,
                % booleans
                <<"crop">>,
                <<"extent">>,
                <<"flip">>,
                <<"flop">>,
                <<"grey">>,
                <<"mono">>,
                <<"lossless">>,
                <<"upscale">>,
                <<"use_absolute_url">>
            ],
            Props = lists:foldl(fun(Key, Acc) ->
                [add_property(MediaId, Key, Data, Context)|Acc]
            end, [], PropKeys),
            Props1 = lists:filter(fun(P) -> P =/= [] end, Props),
            Props2 = case HighResolution of 
                false -> Props1;
                true ->
                    Width = proplists:get_value(width, Props1),
                    Height = proplists:get_value(height, Props1),
                    [
                        {width, Width * 2},
                        {height, Height * 2},
                        {upscale, true}
                        | proplists:delete(height, proplists:delete(width, Props1))]
            end,
            ImageUrl = case z_media_tag:url(MediaId, Props2, Context) of
                {ok, Url} -> Url;
                {error, _} -> undefined
            end,
            {ok, ImageUrl}
    end.


check_required(Data) ->
    case proplists:is_defined(<<"media_id">>, Data) and proplists:is_defined(<<"width">>, Data) of
        true -> ok;
        false -> {error, "Required attribute 'media_id' or 'width' missing"}
    end.

% integers
add_property(_Id, Key, Data, _Context) when
    Key =:= <<"height">>;
    Key =:= <<"quality">>;
    Key =:= <<"width">> ->
    case proplists:get_value(Key, Data) of
        undefined -> [];
        Value -> {z_convert:to_atom(Key), z_convert:to_integer(Value)}
    end;

% strings
add_property(_Id, Key, Data, _Context) when
    Key =:= <<"background">>;
    Key =:= <<"blur">>;
    Key =:= <<"mediaclass">> ->
    case proplists:get_value(Key, Data) of
        undefined -> [];
        Value -> {z_convert:to_atom(Key), Value}
    end;

% booleans
add_property(_Id, Key, Data, _Context) when 
    Key =:= <<"extent">>;
    Key =:= <<"flip">>;
    Key =:= <<"flop">>;
    Key =:= <<"grey">>;
    Key =:= <<"lossless">>;
    Key =:= <<"mono">>;
    Key =:= <<"use_absolute_url">> ->
    case proplists:get_value(Key, Data) of
        undefined -> [];
        Value -> {z_convert:to_atom(Key), z_convert:to_bool(Value)}
    end;

% conditional
add_property(Id, Key, Data, Context) when 
    Key =:= <<"crop">> ->
    KeyAtom = z_convert:to_atom(Key),
    case proplists:get_value(Key, Data) of
        undefined -> [];
        1 -> {KeyAtom, true};
        0 -> [];
        <<"auto">> ->
            case m_rsc:p(Id, crop_center, Context) of
                undefined -> {KeyAtom, false};
                <<>> -> {KeyAtom, false};
                _ -> {KeyAtom, true}
            end;
        Value -> {KeyAtom, Value}
    end;

add_property(_Id, _Key, _Data, _Context) -> [].
    