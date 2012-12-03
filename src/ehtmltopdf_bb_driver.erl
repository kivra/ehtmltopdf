-module(ehtmltopdf_bb_driver).

-export([new/1, run/4]).

new(_) ->
    application:start(ehtmltopdf),
    PrivDir = code:priv_dir(ehtmltopdf),
    {ok, _} = file:copy(PrivDir++"/mess.svg", "/tmp/mess.svg"),
    {ok, _HTML} = file:read_file(PrivDir++"/example.html").

run(convert, _KeyGen, _ValueGen, HTML) ->
    {ok, _PDF} = ehtmltopdf:convert(HTML),
    {ok, HTML}.
