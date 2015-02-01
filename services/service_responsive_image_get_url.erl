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
    responsive_image:get_url(Data, Context).