# Part 1: Proof of Concept

Before starting a largish application, it's a good idea to quickly create a mockup and to nail down the basic design. The tool library app is pretty simple, and there are only a few requirements that need to be considered, and a model that is simple but allows for changes later would be ideal.

The first thing to consider is the data, eventually these will be moved to a persistent storage area, but for the purposes of the mockup `dynamic/1` will be used to create changeable predicates to store data in the Prolog database.

There are a data structures required, the first is the a tool, which I'm going to add an identifier, a type, make, and model as attributes so that a specific tool can be identified easily. Then to cater for the 'borrowing' of tools, we need to know if a tool is in the borrowed state or if it is available. If a tool is borrowed, then who borrowed it and probably when it was borrowed should also be recorded.

```prolog
    % tool(ToolId, Type, Make, Model).
    :- dynamic(tool/4).

    % tool_available(ToolId).
    :- dynamic(tool_available/1).

    % tool_borrowed(ToolId, Who, When).
    :- dynamic(tool_borrowed/3).
```

Ok, so there are some base predicates that can be used to store data, and at the moment they will only exist in memory, which will work for the POC (proof of concept).

There are no tools at the moment, for a quick and dirty, some facts about tools could be added, and I started along that route, but it gets a bit messy because for each `tool/4` fact, there needs to be a matching `tool_available/1` fact. So a quick API to create a tool is required.

    api_add_tool(Type, Make, Model, ToolId) :-
        unique_tool_id(ToolId),
        assert(tool(ToolId, Type, Make, Model)),
        assert(tool_available(ToolId)).

This will insert both the `tool/4` and the `tool_available/1`. The next thing to do is generate the unique Id. There is the [gensym/2](https://www.swi-prolog.org/pldoc/doc_for?object=gensym/2) library for SWI-Prolog that will generate a sequence of numbers, but the issue with this is the number is reset every time the app is restarted, so instead a search for the highest number will be done to get the next highest id.

    tool(ToolId, tool(ToolId, Type, Make, Model)) :-
        tool(ToolId, Type, Make, Model).

    unique_tool_id(ToolId) :-
        aggregate(max(TId), T^tool(TId, T), MaxId),
        ToolId is MaxId + 1, !
        ;
        ToolId = 1.

To help with this, a wrapper for the `tool/4` predicate is created so that a tool can be selected with all attributes as a single item. `aggregate/3` is handy to get the highest ToolId and then to get a unique id, just add 1. The only syntax that might be a bit confusing here is the `T^tool(TId, T)` part. This will get the `aggregate/3` call to ignore the tool structure. Another way to do this is to create a predicate that just gets the ToolId, but the `tool/2` is useful for other predicates.

Now the last thing to do is to add the api methods that fulfil the requirements. These are all fairly self explanatory.

> Users of the system must be able to view a list of all available tools

    api_available_tool(available_tool(Tool)) :-
        tool_available(ToolId),
        tool(ToolId, Tool).

> A tool librarian must be able to view tools that are booked out and to whom they are booked.

    api_borrowed_tool(borrowed_tool(Tool, Who, When)) :-
        tool_borrowed(ToolId, Who, When),
        tool(ToolId, Tool).

> An tool librarian must be able to book a tool and assign it to a user.

    api_borrow_tool(ToolId, Who) :-
        get_time(T),
        tool_available(ToolId),
        assert(tool_borrowed(ToolId, Who, T)),
        retract(tool_available(ToolId)).

> An tool librarian must be able to return a tool that is booked.

    api_return_tool(ToolId) :-
        tool_borrowed(ToolId, _, _),
        assert(tool_available(ToolId)),
        retract(tool_borrowed(ToolId, _, _)).

Ok, now everything is created and some tests can be done to see if we can do everything that is required.

First add some tools:

    ?- api_add_tool(hammer, 'Maxwell', '7" heavy', Id).
    Id = 1.

    ?- api_add_tool(spade, 'Shovels''r''us', 'Wide blade, dirt machine', Id).
    Id = 2.

    ?- api_add_tool(drill, 'Markita', 'Hammer Drill no.4', Id).
    Id = 3.

Now check that the tools are available:

    ?-  api_available_tool(T).
    T = available_tool(tool(1, hammer, 'Maxwell', '7" heavy')) ;
    T = available_tool(tool(2, spade, 'Shovels\'r\'us', 'Wide blade, dirt machine')) ;
    T = available_tool(tool(3, drill, 'Markita', 'Hammer Drill no.4')).

Looks like all tools are present and correct, but they shouldn't be borrowed at this point either:

    ?- api_borrowed_tool(T).
    false.

Correct. Time to borrow a tool, Fred needs a hammer, so he can go first.

    ?- api_borrow_tool(1, 'Fred').
    true.

    ?- api_borrowed_tool(T).
    T = borrowed_tool(tool(1, hammer, 'Maxwell', '7" heavy'), 'Fred', 1600848776.8166406).

The tool even shows up as borrowed, nice. Is it still available?

    ?-  api_available_tool(T).
    T = available_tool(tool(2, spade, 'Shovels\'r\'us', 'Wide blade, dirt machine')) ;
    T = available_tool(tool(3, drill, 'Markita', 'Hammer Drill no.4')).

Nope, excellent, let's return the tool then and it should come back.

    ?- api_return_tool(1).
    true.

    ?-  api_available_tool(T).
    T = available_tool(tool(2, spade, 'Shovels\'r\'us', 'Wide blade, dirt machine')) ;
    T = available_tool(tool(3, drill, 'Markita', 'Hammer Drill no.4')) ;
    T = available_tool(tool(1, hammer, 'Maxwell', '7" heavy')).

    ?- api_borrowed_tool(T).
    false.

Ok, all working, there are obviously more intense testing that needs to be done, but that's an exercise for the reader.
