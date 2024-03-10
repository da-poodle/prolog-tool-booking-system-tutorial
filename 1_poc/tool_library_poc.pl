
% tool(ToolId, Type, Make, Model).
:- dynamic(tool/4).

% tool_available(ToolId).
:- dynamic(tool_available/1).

% tool_borrowed(ToolId, Who, When).
:- dynamic(tool_borrowed/3).

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
    unique_tool_id(ToolId),
    assert(tool(ToolId, Type, Make, Model)),
    assert(tool_available(ToolId)).  

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
    tool_available(ToolId),
    assert(tool_borrowed(ToolId, Who, T)),
    retract(tool_available(ToolId)).

% a tool is returned
api_return_tool(ToolId) :-
    tool_borrowed(ToolId, _, _),
    assert(tool_available(ToolId)),
    retract(tool_borrowed(ToolId, _, _)).
