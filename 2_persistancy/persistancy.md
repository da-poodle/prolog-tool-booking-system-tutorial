# Part 2: Adding Persistent Data

Now that a bit of a base for the tool library app is in place, it's time to get serious and store data somewhere. Rather than going all out and using a database, there is a simpler option in SWI-Prolog with the [persistency library](https://www.swi-prolog.org/pldoc/man?section=persistency). This library works with a simple file based database that reflects the Prolog database. So if you add a fact in Prolog, the file will also be updated etc. All the facts that are going to be persisted need to be in a module, so the first thing to do is create a module for the API.

    :- module(tool_library_api, [
        api_add_tool/4,
        api_available_tool/1,
        api_borrowed_tool/1,
        api_borrow_tool/2,
        api_return_tool/1
    ]).

This module is an interface to the API methods that were created in the POC and it allows the use of the persistency library, but also hides away the implementation so that the type of persistance could change later if required.

To use the persistency library, the terms that are going to be persistable need to be explicitly defined, and these definitions will replace the `dynamic` definitions in the POC.

    :- use_module(library(persistency)).

    :- persistent
        tool(id:integer, type:atom, make:atom, model:atom),
        tool_available(id:integer),
        tool_borrowed(id:integer, who:atom, when:float).

This definition adds some type checking as well, which is enforced using the `must_be/2` predicate. The persistency library will also creates `assert_<predicate_name>` and `retract_<predicate_name>` for each type that we define, plus some other predicates which won't be used.

The APIs need to change slightly, because `assert/1` and `retract/1` will be replaced by specific calls for each type. Also one more change is to add thread safety around the adding and removing of data, to stop a tool being borrowed twice or some other error. This is required as the app could eventually be used by multiple people at once. Mutexes will achieve this and only the APIs that update data will need to be altered.

The add tool requires a mutex to prevent the same ID being used for two tools. `assert/1` is replaced by `assert_tool/4` and `assert_tool_available/1` which are created by the persistency library.

    api_add_tool(Type, Make, Model, ToolId) :-
        with_mutex(create_tool, (
            unique_tool_id(ToolId),
            assert_tool(ToolId, Type, Make, Model),
            assert_tool_available(ToolId)
        )).

The borrow tool API needs to prevent the same tool being borrowed more than once. `assert/1` is replaced by `assert_tool_borrowed/3` and `retract/1` is replaced by `retract_tool_available/1`.

    api_borrow_tool(ToolId, Who) :-
        get_time(T),
        with_mutex(tools, (
            tool_available(ToolId),
            assert_tool_borrowed(ToolId, Who, T),
            retract_tool_available(ToolId)
        )).

The return tool needs to prevent the same tool being made available more than once. `assert/1` is replaced by `assert_tool_available/1` and `retract/1` is replaced by `retract_tool_borrowed/3`.

    api_return_tool(ToolId) :-
        with_mutex(tools, (
            tool_borrowed(ToolId, _, _),
            assert_tool_available(ToolId),
            retract_tool_borrowed(ToolId, _, _)
        )).

Now the persistency is almost set up, but the last thing to do is to create the file, which will be done using the following code. There are several [sync options](https://www.swi-prolog.org/pldoc/doc_for?object=db_sync/1) but the close option is good for development and it means that the file is updated and closed every time an update is made.

    attach_tool_db :-
        db_attach('tools.db', [sync(close)]).

`attach_tool_db/0` must be called from outside the persistency module (for reasons I don't understand). This is a good opportunity to create a `load.pl` file which will be the core file that loads all the modules required the app and setup any services that need to be run. It is pretty simple at the moment:

    :- use_module(tool_library_api).

    :- tool_library_api:attach_tool_db.

Now running the program using the command `swipl load.pl` will create a `tools.db` file that is going to contain the data. So the last thing to do is add the tools from before.

    ?- api_add_tool(hammer, 'Maxwell', '7" heavy', Id).
    Id = 1.

    ?- api_add_tool(spade, 'Shovels''r''us', 'Wide blade, dirt machine', Id).
    Id = 2.

    ?- api_add_tool(drill, 'Markita', 'Hammer Drill no.4', Id).
    Id = 3.

This looks exactly the same as when using the POC but the difference is that the `tools.db` has some entries in it:

    created(1600904873.3848188).
    assert(tool(1,hammer,'Maxwell','7" heavy')).
    assert(tool_available(1)).
    assert(tool(2,spade,'Shovels\'r\'us','Wide blade, dirt machine')).
    assert(tool_available(2)).
    assert(tool(3,drill,'Markita','Hammer Drill no.4')).
    assert(tool_available(3)).

The other APIs work as well and update the data, giving a full audit trace for free (until the data is vacuumed!). It's a simple way to store data, but effective for a small usage app, and the data is easy to backup as well (it's just a text file).

Ok, so now the app is kind of usable from the command line if you want to type Prolog predicates to use it, but it's time to create a more friendly user interface!
