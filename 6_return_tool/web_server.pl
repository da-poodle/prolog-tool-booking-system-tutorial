:- module(web_server, [start_server/1]).

% create the web server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_authenticate)).
:- use_module(api_user).

start_server(Port) :- 
    http_server(auth_dispatch, [port(Port)]).

auth_dispatch(Request) :-
(   http_authenticate(basic('users.txt'), Request, Fields)
->  (
        userfile_user(Fields, User), 
        http_dispatch([User|Request])
    )
    ;  
     throw(http_reply(authorise(basic, tool_library)))
).