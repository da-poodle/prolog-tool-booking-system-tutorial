:- module(web_server, [start_server/1]).

% create the web server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

start_server(Port) :- 
    http_server(http_dispatch, [port(Port)]).