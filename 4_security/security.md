Up: [Tool Library Contents](/content/tool_library)<br />
Previous: [Creating the first web page](/content/tool_library/first_web_page)

### Part 4: Adding Security

In the last post the first view was created that showed a list of tools, but you still have to borrow and return tools by using the back end. Before adding these operations to the web page, it is worth having a look at security so that it doesn't require a lot of changes afterwards. 

To keep things simple, basic authentication will be used, which is easy to do with SWI-Prolog. Basic authenticaion, just needs a username and password, and the prompt to enter your credentials is handled by the browser, so no page is required. To add in security there are two new files, and a few minor modifications: 

- api_user.pl (new)
- users.txt (new)
- api_tool_library.pl
- load.pl
- tools.db
- web_index.pl (modify)
- web_server.pl (modify)

Basic auth uses a password file (users.txt), which is a text file that has a list of fields separated by a colon (:). This is similar to the UNIX format for password files. You can add extra fields to a user record and this feature will be made use of. A password entry will look like the following: 

    <username>:<password>:<full name>:<email>:<list or roles>

For example, one of the users I have setup for testing is 'Chalice Sheridan' and their record looks like the following:

    csheridan:pjBN6RLmumeAk:Chalice Sheridan:csheridan@toollib.com:return,borrow,view

All of the users have randomly generated names from [this site](https://www.behindthename.com/random/) if you are interested (they are not real people!) and the passwords are set to be the username, just for convienience. 

To generate the password, the ``crypt/2`` predicate is used. 

    ?- crypt(csheridan, Cypher), atom_codes(Password, Cypher).
    Cypher = [83, 72, 104, 65, 105, 51, 114, 86, 100|...],
    Password = 'SHhAi3rVd5wMY'.

The password can be anything, remember the username is used here just for testing! The ``crypt/2`` predicate takes the plain text, and creates a hash of the text which is stored as the password. The hash is different each time ``crypt/2`` is run, but when comparing any of the hashes can be compared to the plain text. 

    ?- crypt(csheridan, 'SHhAi3rVd5wMY').
    true.

    ?- crypt(a_wrong_password, 'SHhAi3rVd5wMY').
    false.

Now that users have been created, a check needs to be put into the website to make sure that the user is checked for when a page is loaded. The easiest way I have found to do this is to create a new router that intercepts the request before it is dispatched, and then use http_dispatch manually if the logon succeeds. 

This requires some modifications to the ``web_server.pl`` file.

    ...
    :- use_module(api_user).

    start_server(Port) :- 
        http_server(auth_dispatch, [port(Port)]).

    auth_dispatch(Request) :-
    (   http_authenticate(basic('users.txt'), Request, Fields)
        ->  (
            % convert the fields from the users.txt into a user/4 term.
            userfile_user(Fields, User), 

            % call http_dispatch manually after adding the user to the request.
            http_dispatch([User|Request])
        )
        ;  
        throw(http_reply(authorise(basic, tool_library)))
    ).

Firstly, a new module ``api_user`` will be included which is defined below, and also the ``http_server/2`` predicate will call our custom dispatch predicate instead of calling ``http_dispatch`` directly. 

In the ``auth_dispatch`` predicate, the ``http_authenticate/3`` predicate will handle the reading of the user file and checking the password. The ``Fields`` variable contains a list of all the fields in the users file except the password for the authenticated user.  

If the authentication fails, either due to no credentials being sent on the request, or the credentials being wrong, then an authentication challenge is sent back to the browser, which makes the browser pop up a username/password dialog. 

![The Browser Login Prompt](/images/tool_library_login_prompt.PNG)

The user data structure has been abstracted into a new module ``api_user.pl`` with several helper methods that will be used. It looks like the following: 

    :- module(api_user, [
        userfile_user/2,
        request_user/2,
        verify_user_role/2,
        user_username/2,
        user_name/2,
        user_email/2,
        user_role/2
    ]).

    % convert the user from the password file into a term
    userfile_user([UserName, Name, Email, Roles], user(UserName,Name,Email,RoleList)) :-
        atomic_list_concat(RoleList, ',', Roles).

    % search for a user object on the request (the request is just a list of data)
    request_user(Request, User) :-
        User = user(_,_,_,_),
        memberchk(User,Request).

    % make sure that a user has role or throw forbidden status
    verify_user_role(User, Role) :-
        user_role(User, Role) -> true
        ;
        throw(http_reply(forbidden('this location'))).

    % the username of a user
    user_username(user(UserName,_,_,_), UserName).

    % the full name of a user
    user_name(user(_,Name,_,_), Name).

    % the email of a user
    user_email(user(_,_,Email,_), Email).

    % holds if user has role
    user_role(user(_,_,_,RoleList), Role) :-
        memberchk(Role, RoleList).

I won't go into the contents of the file in detail, except to say that this method of abstracting an data type into a new module is useful because it means that you don't have to expose the structure of the type to the rest of the code. Then if the ``user/4`` type requires a new field to be added or there is another change, only the ``api_user.pl`` file needs to change. 

Finally, we need to update the home page to check that only users with the view role can see the list of tools, and to print a welcome message. All of the changes are in the http handler method.

    ...

    :- use_module(api_user).

    :- http_handler(root(.), index, [id(home)]).
    index(Request) :-

        % get the user from the request
        request_user(Request, User),

        % make sure that the user can view the site
        verify_user_role(User, view),

        ...

        % use the api_user helper to get the users full name from the user/4 type
        user_name(User, UserName),

        reply_html_page(
            title('Tool Library'), 
            div([
                h2('Available Tools'),
                % add a new P element with a welcome message, with a class of 'userName'
                p(class=userName, ['Welcome ', UserName]),
                div(Table)
            ])).
        
        ...

As an aside, you can add attributes to any HTML element by using the first argument. If there are multiple attributes, then a list is used, but a single element doesn't require a list. eg: 

    % one attribute requires no list
    % one part to the body requires no list.
    p(class=c1, 'some text').
   
    % result = <p class="c1">some text</p>

    % multiple attributes or elements require lists
    p([class=c1, title="hover me to see"], [b('bold'), ' text']).

    % result = <p class="C1" title="hover me to see"><b>bold</b> text</p>

With these changes complete, now when you log in there is a nice welcome message to tell you who is logged in.

![Welcome Message](/images/tool_library_login_message.PNG)

And if a user doesn't have permission to view, then the following page is shown

![Forbidden Message](/images/tool_library_login_forbidden.PNG)

This forbidden page, or any other status code, can be customised but that is out of scope for this project. You might have seen a custom 404 page on this site at some point! 

The level of security is not 'fantastic' but it is simple to setup and good enough for the use case. These changes allow only certain people to have access to the functions that will allow tools to be borrowed and returned. 

In the next post, the booking tool feature will be added. 

Next: [Book Tool](/content/tool_library/book_tool)
