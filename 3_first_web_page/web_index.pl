:- module(web_index, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(api_tool_library).

:- http_handler(root(.), index, [id(home)]).
index(_Request) :-
    % find all the tools to display
    findall(Tool, api_available_tool(Tool), Tools),

    % generate the table
    tools_table(Tools, Table),

    % return the html page
    reply_html_page(
        title('Tool Library'), 
        div([
            h2('Available Tools'), 
            div(Table)
        ])).

% Create a table of tools
tools_table(Tools, Table) :-
    maplist(tool_row, Tools, TBody),
    tools_table_(TBody, Table).

tools_table_([], 'No Tools To Show').
tools_table_([T|Tt], Table) :-
    Table = table(cellpadding=3, [
        thead([
            th('Tool Id'),
            th('Tool Name'),
            th('Manufacturer'),
            th('Model')
        ]),
        tbody([T|Tt])
    ]).

tool_row(available_tool(tool(Id, Type, Make, Model)), Row) :-
    Row = tr([
        td(Id),
        td(Type),
        td(Make),
        td(Model)
    ]).
