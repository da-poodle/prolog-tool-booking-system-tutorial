:- module(web_index, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(api_tool_library).
:- use_module(api_user).
:- use_module(web_components).

:- http_handler(root(.), index, [id(home)]).
index(Request) :-

    % get the user from the request
    request_user(Request, User),
    verify_user_role(User, view),

    % find all the tools to display
    findall(Tool, (
        api_available_tool(Tool)
        ; api_borrowed_tool(Tool)
    ), Tools),

    sort(1, <, Tools, SortedTools),

    % generate the table
    tools_table(SortedTools, User, Table),

    % return the html page
    reply_with_layout('Available Tools', User, div(Table)).

% Create a table of tools
tools_table(Tools, User, Table) :-
    maplist(tool_row(User), Tools, TBody),
    tools_table_(TBody, Table).

tools_table_([], 'No Tools To Show').
tools_table_([T|Tt], Table) :-
    Table = table([
        thead([
            th('Tool Id'),
            th('Tool Name'),
            th('Manufacturer'),
            th('Model'),
            th('')
        ]),
        tbody([T|Tt])
    ]).

tool_row(User, available_tool(Tool), Row) :-
    Tool = tool(ToolId, _, _, _),
    available_action_column(User, ToolId, ActionCol),
    tool_table_row(Tool, ActionCol, Row).

tool_row(User, borrowed_tool(Tool, _, _), Row) :-
    Tool = tool(ToolId, _, _, _),
    booked_action_column(User, ToolId, ActionCol),
    tool_table_row(Tool, ActionCol, Row).

tool_table_row(tool(Id, Type, Make, Model), ActionCol, Row) :-
    Row = tr([
        td(Id),
        td(Type),
        td(Make),
        td(Model),
        td(ActionCol)
    ]).

available_action_column(User, ToolId, span(class=btn, Btn)) :- 
    user_role(User, book) 
    -> (
        http_link_to_id(book_tool, [tool_id(ToolId)], HREF),
        Btn = a([href=HREF, class='book-btn'], 'Book')
    )
    ;
    Btn = span(class=noperm, 'Available').

booked_action_column(User, ToolId, span(class=btn, Btn)) :- 
    user_role(User, return) 
    -> (
        http_link_to_id(return_tool, [tool_id(ToolId)], HREF),
        Btn = a([href=HREF, class='return-btn'], 'Return')
    )
    ;
    Btn = span(class=noperm, 'Booked').