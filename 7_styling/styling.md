Up: [Tool Library Contents](/content/tool_library)<br />
Previous: [Return Tool](/content/tool_library/return_tool)

Now that the basic functions have been added, then it is time to make the site look a bit better. There are only a few real changes that are required, a CSS file is added, the page has a 'head' section added, and there are a few changes to styles and HTML content to allow for styling. 

Here is a summary of the files that will change:

- api_tool_library.pl
- api_user.pl
- load.pl
- site.css (new)
- tools.db
- users.txt
- web_book_tool.pl (modify)
- web_return_tool.pl (modify)
- web_components.pl (modify)
- web_index.pl (modify)
- web_server.pl (modify)

To add the CSS file, the web server needs to be told that the file exists and it is allowed to serve it. The ``site.css`` file will be located in the same directory as everything else, and this is ok because a directive can be added to allow only that file to be served. This is put in the ``webserver.pl`` file. 

    :- http_handler(root('site.css'), http_reply_file('site.css', []), []).

This directive says that if someone asks for '/site.css' then it is located in the current path, and it can be served. This is the same as a normal ``http_handler``, but returns a file instead of prolog content. 

The file can be read by the browser now, but it is isn't included in the page. To do this a refactor will be done, because I want to add the link to the file in the page ``<head>`` section. 

The first thing to do is create a predicate in the ``web_components.pl`` file to allow for a consistent layout on each page. 

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

The layout contains the title, a viewport instruction (so that the browser knows how to size the page), a link to the stylesheet, the page heading, and a place to put the main content of the page. 

Now that this is created, we can update each page to use the template. 

The main page that draws the tool table:

    ...
    
    % Add the tool table as the body of the page
    reply_with_layout('Available Tools', User, div(Table)).

    ...

The tool booking page:

    ...

    % Add the tool description and the list of users as the body
    reply_with_layout('Book Tool', User, [
        \tool_description(Tool),
        div([p('Choose who is booking the tool'), UserList])    
    ]).

    ...

And finally, the return tool page:

    ...

    % This page has the tool description, the booking description 
    % and a button to process the tool return. 
    reply_with_layout('Return Tool', User, [
        \tool_description(Tool), 
        p(BookingDesc),
        div(class=btns, a([class='btn', href=ToolIsReturnedLink], 'Return Tool'))
    ]).

    ...

That is the main changes! There are a few minor changes, like adding styles to elements, but there are too many and they are not Prolog, but rather HTML changes in Prolog, so I am going to skip them. 

Instead it is worth talking a bit about CSS. I am not an expert on CSS, but feel it is important to create a good color set to work with. There are many sites that will generate a color pallette for you, and so I used http://coolors.co to create one, and luckily the site provides a link that can be used to show you! https://coolors.co/503d42-f5fbef-dbb3b1-6c534e-2c1a1d

I also choose the 'Plus Jakarta Sans' font from Google Fonts, to make the content a bit easier to read. It is good practice to use CSS variables to abstract the colors away from the rest of the styles, so the top of the stylesheet does this: 

    @import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@200&display=swap');

    :root {
        --font-family: 'Plus Jakarta Sans', sans-serif; 
        --heading-background: #503D42;
        --heading-text-color: #F5FBEF;
        --highlight-color: #dbb3b1ff;
        --link-color: #6c534eff;
        --text-color: #2c1a1dff;
    }

If you haven't used CSS variables before, it is worth learning, they allow quick color updates, as long as the names you use for the variable are relevant and consistant. To use a variable in a style, then the following syntax is used: 

    td {
        color: var(--text-color);
        padding: 0.5rem;
        text-align: left;
        border-bottom: 1px solid var(--highlight-color);
    }

So a table cell will use the ``--text-color`` from the variable for the font color, and the ``--hightlight-color`` for the border color.

The stylesheet is not complicated, it is better to actually look at the results: 

The table page now looks like the following: 

![Table Page after being styled](/images/tool_library_styled_table2.PNG)

The booking page: 

![Booking Page after being styled](/images/tool_library_styled_book.PNG)

And the return tool page: 

![Booking Page after being styled](/images/tool_library_styled_return.PNG)

And that is it, the tool library could potentially be used now as a MVP (minimal viable product). There are a lot of enhancements that can be made, like user management, tool management, sorting, search, etc.. 

If I ever run out of more interesting Prolog things to post about then that will be the time for that kind of thing! 