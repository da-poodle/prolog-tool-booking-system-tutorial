Up: [Tool Library Contents](/content/tool_library)<br />
Previous: [Adding Persistent Data](/content/tool_library/persistency)

### Part 3: Creating the first web page

In the previous post, the Proof of Concept (POC) was taken and made into a more usable (but still simple) api that can store data for our app. Now it is time to start working on displaying the data so that the tool library can actually be used. There are several ways to create a web page, I'm going to stick with the simple version and use plain Prolog just for the sake of the exercise. The site won't be as pretty but the purpose it to demonstrate how Prolog web pages work.

To start with let's outline the file structure that will be used now, so far there are two files, but a couple more need to be created, one for the web server, and one for our first page, the index page. 

- api_tool_libary.pl
- load.pl
- web_index.pl
- web_server.pl

To create a basic web server is a pretty simple process, it is good to separate this out into a new module and then you can use the same server for every project. 

> Contents of file: web_server.pl

    :- module(web_server, [start_server/1]).

    :- use_module(library(http/thread_httpd)).
    :- use_module(library(http/http_dispatch)).

    % create the web server
    start_server(Port) :- 
        http_server(http_dispatch, [port(Port)]).

There is one predicate, which is to start the server on a specified port. If you load this file and call ``start_server(5000)`` then a webserver will be created on port 5000, but it doesn't do anything useful yet because we have no http handlers. 

Before creating a handler, let's create the code to output all the available tools in a html table. This code lives in the ``web_index.pl`` file.

    % Create the table, including header
    tools_table_([], 'No Tools To Show').
    tools_table_(TableBody, Table) :-
        TableBody = [T|Tt],
        Table = table([
            thead([
                th('Tool Id'),
                th('Tool Name'),
                th('Manufacturer'),
                th('Model')
            ]),
            tbody(TableBody)
        ]).

A table is specified as a set of terms, this will translate to HTML by using the phrase/2 predicate, which we will see in a moment.

    % Generate a row
    tool_row(available_tool(tool(Id, Type, Make, Model)), Row) :-
        Row = tr([
            td(Id),
            td(Type),
            td(Make),
            td(Model)
        ]).

Each table row is simply a term as well, with ``tr/1`` and inside a list of ``td/1`` terms. Note that the ``available_tool/1`` structure is passed in to this predicate to get the data about our tool.

    % Map the available tools to the table
    tools_table(Tools, Table) :-
        maplist(tool_row, Tools, TBody),
        tools_table_(TBody, Table).

The last thing to do here is to map the list of tools into a list of table rows, and pass that to the predicate that creates the table. To test this works, run the following. 

    swipl .\web_index.pl
    ?- use_module(api_tool_library).
    true
    
    ?- api_tool_library:attach_tool_db.
    true

    ?- findall(Tool, api_available_tool(Tool), Tools), tools_table(Tools, Table).
    Tools = [available_tool(tool(1, hammer, 'Maxwell', '7" heavy')), available_tool(tool(2, spade, 'Shovels\'r\'us', 'Wide blade, dirt machine')), available_tool(tool(3, drill, 'Markita', 'Hammer Drill no.4')), available_tool(tool(4, screwdriver, phillips, '3mm phillips head'))],
    Table = table([thead([th('Tool Id'), th('Tool Name'), th('Manufacturer'), th('Model')]), tbody([tr([td(1), td(hammer), td(...)|...]), tr([td(2), td(...)|...]), tr([td(...)|...]), tr([...|...])])]).

The output is not that useful, and we kind of want to see what the HTML will look like so run the following: 

    ?- use_module(library(http/html_write)).
    ?- phrase(html($Table), HTML, []), print_html(HTML).

    <table cellpadding="3">
    <thead><th>Tool Id</th><th>Tool Name</th><th>Manufacturer</th><th>Model</th></thead><tbody>
    <tr><td>1</td><td>hammer</td><td>Maxwell</td><td>7" heavy</td></tr>
    <tr><td>2</td><td>spade</td><td>Shovels'r'us</td><td>Wide blade, dirt machine</td></tr>
    <tr><td>3</td><td>drill</td><td>Markita</td><td>Hammer Drill no.4</td></tr>
    <tr><td>4</td><td>screwdriver</td><td>phillips</td><td>3mm phillips head</td></tr>
    </tbody>
    </table>

    HTML = [nl(2), <, table, ' ', cellpadding, '="', 3, '"', >|...],
    Table = table(cellpadding=3, [thead([th('Tool Id'), th('Tool Name'), th('Manufacturer'), th('Model')]), tbody([tr([td(1), td(hammer), td(...)|...]), tr([td(2), td(...)|...]), tr([td(...)|...]), tr([...|...])])]).

Looks like it is working! Note that you can use a value from a previous query by putting the '$' in front of the name in the CLI. Also to print out the html, I included the ``http/html_write`` library, which gives access to the ``print_html/1`` predicate. 

The next thing to do is add the http handler.

    :- module(web_index, []).

    :- use_module(library(http/http_dispatch)).
    :- use_module(library(http/html_write)).
    :- use_module(api_tool_library).

Create a module and add the imports we need. The ``http/http_dispatch`` library is for the ``http_handler/3`` predicate, and the ``http/html_write`` library is for the ``reply_html_page/1`` predicate.

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

The handler is defined at 'root(.)', this is the URI that will trigger this handler. In this case root(.) means that the page shown when no path is specified. the ``id(home)`` is an option to the http handler, it is optional, but if we include an id then it is easier to link to the page later, so as common practice I tend to include it.

Next we run the query that we ran before and create the table into a structure. 

The final step is to return a page, which has a title, and a heading and our table. 

The handler code is done, but it won't work just yet because we need to load the server and the handler together. To do this the load.pl file needs to be updated to include the necessary modules, and start the server. 

> Contents of file: load.pl

    :- use_module(api_tool_library).
    :- use_module(web_index).
    :- use_module(web_server).

    :- api_tool_library:attach_tool_db.

    :- start_server(5022).

ok, so now when we run ``swipl load.pl`` it will print out that a server is started and opening a browser to http://localhost:5022 shows the following page: 

![A list of available tools](/images/tool_library_available_tools.PNG)

It is very simple for now and in the next post the tool library will be expanded to have some basic security, then we can start to add the features that only tool librarians are allowed to have access to.

Next: [Adding security](/content/tool_library/security)
