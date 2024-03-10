:- module(web_book_tool, []).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(api_tool_library).
:- use_module(api_user).
:- use_module(web_components).

:- http_handler(root(book_tool), book_tool, [id(book_tool)]).
book_tool(Request) :-
    request_user(Request, User),
    verify_user_role(User, book),

    http_parameters(Request, [tool_id(ToolId, [integer, optional(false)])]),

    % Find the tool that is being booked, which will be available or failed
    api_tool_library:tool(ToolId, Tool),
    
    % Get a list of users to choose from, choosing the users will book the tool
    user_choice_list(ToolId, UserList),

    % return the html page
    reply_with_layout('Book Tool', User, [
        \tool_description(Tool),
        div([p('Choose who is booking the tool'), UserList])    
    ]).

:- http_handler(root(user_books_tool), user_books_tool, [id(user_books_tool)]).
user_books_tool(Request) :-
    request_user(Request, User),
    verify_user_role(User, book),

    http_parameters(Request, [
        tool_id(ToolId, [optional(false), integer]),
        user(Who, [optional(false), atom])
    ]),

    api_borrow_tool(ToolId, Who),
    http_redirect(see_other, location_by_id(home), Request).

user_choice_list(ToolId, div(class=userlist, UserList)) :-
    users(Users),
    maplist(user_list_row(ToolId), Users, UserList).

user_list_row(ToolId, User, UserListRow) :-
    user_username(User, UserName),
    user_name(User, Name),
    http_link_to_id(user_books_tool, [tool_id(ToolId), user(UserName)], HREF),
    UserListRow = div(a([class=btn, href=HREF], Name)).
