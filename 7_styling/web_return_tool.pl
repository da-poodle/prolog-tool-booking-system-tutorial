:- module(web_return_tool, []).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(api_tool_library).
:- use_module(api_user).
:- use_module(web_components).

:- http_handler(root(return_tool), return_tool, [id(return_tool)]).
return_tool(Request) :-
    request_user(Request, User),
    verify_user_role(User, return),

    http_parameters(Request, [tool_id(ToolId, [integer, optional(false)])]),

    Tool = tool(ToolId, _, _, _),
    api_borrowed_tool(borrowed_tool(Tool, Who, When)),

    http_link_to_id(tool_is_returned, [tool_id(ToolId)], ToolIsReturnedLink),

    user_details(Who, BorrowUser),
    user_name(BorrowUser, Name),
    format_time(atom(WhenFormatted), '%d %b %Y', When),
    format(atom(BookingDesc), 'Tool was booked by ~w on ~w', [Name, WhenFormatted]),

    reply_with_layout('Return Tool', User, [
        \tool_description(Tool), 
        p(BookingDesc),
        div(class=btns, a([class='btn', href=ToolIsReturnedLink], 'Return Tool'))
    ]).

:- http_handler(root(tool_is_returned), tool_is_returned, [id(tool_is_returned)]).
tool_is_returned(Request) :-
    request_user(Request, User),
    verify_user_role(User, return),

    http_parameters(Request, [tool_id(ToolId, [optional(false), integer])]),

    api_return_tool(ToolId),
    http_redirect(see_other, location_by_id(home), Request).

user_details(UserName, User) :-
    users(Users),
    user_username(User, UserName),
    member(User, Users).
