:- use_module(api_tool_library).
:- use_module(web_index).
:- use_module(web_server).

:- api_tool_library:attach_tool_db.

:- start_server(5000).