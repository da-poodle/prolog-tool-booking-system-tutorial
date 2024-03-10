:- module(api_user, [
    userfile_user/2,
    pwd_file_user/2,
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

% convert the entry in a password file to a user data structure
pwd_file_user(passwd(UserName, _, Fields), User) :-
    userfile_user([UserName|Fields], User).

% get the user data from the http request.
request_user(Request, User) :-
    User = user(_,_,_,_),
    memberchk(User,Request).

% user has role or throw forbidden status
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