:- module(web_components, [
    page_heading/4,
    tool_description/3,
    reply_with_layout/3
]).

:- use_module(library(http/html_write)).
:- use_module(api_user).

%
% a heading at the top of the page
%
page_heading(Title, User) -->
    { 
        user_name(User, UserName),
        user_email(User, Email),
        format(atom(WelcomeMsg), 'Welcome ~w (~w)', [UserName, Email]) 
    },
    html(
        div(class=heading, [
            a([href=location_by_id(home)], Title), 
            p(class='welcome-msg', WelcomeMsg)
        ])
    ).

%
% the description of a tool
%
tool_description(tool(ToolId, Type, Make, Model)) -->
    html(
        div(class=tool, [
            h3(['Tool ', ToolId]),
            p([Type, ', ', Make, ', ', Model])
        ])
    ).

reply_with_layout(Title, User, Body) :-
    reply_html_page([
            title('Tool Library'),
            meta([name=viewport, content="width=device-width, initial-scale=1.0"]),
            link([rel=stylesheet,type='text/css',href='/site.css'],[])
        ],
        [
            \page_heading(Title, User), 
            div(class=main, Body)
        ]).