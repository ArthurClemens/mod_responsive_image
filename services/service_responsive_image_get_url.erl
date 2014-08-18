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
            MediaId = z_convert:to_integer(proplists:get_value(<<"media_id">>, Data)),
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
                <<"use_absolute_url">>
            ],
            Props = lists:foldl(fun(Key, Acc) ->
                [add_property(Key, Data)|Acc]
            end, [], PropKeys),
            Props1 = lists:filter(fun(P) -> P =/= [] end, Props),
            ImageUrl = case z_media_tag:url(MediaId, Props1, Context) of
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
add_property(Key, Data) when
    Key =:= <<"height">>;
    Key =:= <<"quality">>;
    Key =:= <<"width">> ->
    case proplists:get_value(Key, Data) of
        undefined -> [];
        Value -> {z_convert:to_atom(Key), z_convert:to_integer(Value)}
    end;
% strings
add_property(Key, Data) when
    Key =:= <<"background">>;
    Key =:= <<"blur">>;
    Key =:= <<"mediaclass">> ->
    case proplists:get_value(Key, Data) of
        undefined -> [];
        Value -> {z_convert:to_atom(Key), Value}
    end;
% booleans
add_property(Key, Data) when 
    Key =:= <<"crop">>;
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
add_property(_Key, _Data) -> [].
    