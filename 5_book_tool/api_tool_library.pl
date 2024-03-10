:- module(tool_library_api, [
   tool/2,
   api_add_tool/4,
   api_available_tool/1,
   api_borrowed_tool/1,
   api_borrow_tool/2,
   api_return_tool/1,
   attach_tool_db/0,
   vacumn_tool_db/0
]).

:- use_module(library(persistency)).

% define the terms that will be persisted
:- persistent
    tool(id:integer, type:atom, make:atom, model:atom),
    tool_available(id:integer),
    tool_borrowed(id:integer, who:atom, when:float).

% Helper to map all the tool fields to one object
tool(ToolId, tool(ToolId, Type, Make, Model)) :- 
    tool(ToolId, Type, Make, Model).

% Add a new tool
unique_tool_id(ToolId) :-
    aggregate(max(TId), T^tool(TId, T), MaxId),
    ToolId is MaxId + 1, !
    ; 
    ToolId = 1.
    
api_add_tool(Type, Make, Model, ToolId) :-
    with_mutex(create_tool, (    
        unique_tool_id(ToolId),
        assert_tool(ToolId, Type, Make, Model),
        assert_tool_available(ToolId)
    )).  

% get tools that are available
api_available_tool(available_tool(Tool)) :-
    tool_available(ToolId),
    tool(ToolId, Tool).

% get tools that are borrowed
api_borrowed_tool(borrowed_tool(Tool, Who, When)) :-
    tool_borrowed(ToolId, Who, When),
    tool(ToolId, Tool).

% a user borrows a tool
api_borrow_tool(ToolId, Who) :-
    get_time(T),
    with_mutex(tools, (
        tool_available(ToolId),
        assert_tool_borrowed(ToolId, Who, T),
        retract_tool_available(ToolId)
    )).    

% a tool is returned
api_return_tool(ToolId) :-
    with_mutex(tools, (    
        tool_borrowed(ToolId, _, _),
        assert_tool_available(ToolId),
        retract_tool_borrowed(ToolId, _, _)
    )).

% database predicates
attach_tool_db :-	
	db_attach('tools.db', [sync(close)]).

vacumn_tool_db :-
	db_sync(gc(always)).
