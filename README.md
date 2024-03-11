This series of posts comes from a story that happened a couple of years ago. Where I was working at the time, there was a tool library system which allowed workers to book out a tool and then return it later rather then buying all the equipment they needed. This system was overly complicated and as a result didn't work half the time.

So we looked at it and thought, for what the system does, it would actually be quicker to rewrite a simple replacement system than fix what was there! This is often the thought that developers have, but in this case it was fixed and forgotten about.

Now I have rewritten the site in Prolog as a very simple, but functional system. The following functions must be present:

- Users of the system must be able to view a list of all available tools.
- A tool librarian must be able to book a tool and assign it to a user.
- A tool librarian must be able to return a tool that is booked.
- A tool librarian must be able to view tools that are booked out and to whom they are booked.

That is pretty much it, but this is a larger project than the other posts I have done so it is split into several progressive sections.

1. [Proof Of Concept](1_poc/poc.md)
1. [Adding Persistent Data](2_persistancy/persistancy.md)
1. [Creating the first web page](3_first_web_page/first_web_page.md)
1. [Adding security](4_security/security.md)
1. [Implement booking of a tool](5_book_tool/book_tool.md)
1. [Implement returning of a tool](6_return_tool/return_tool.md)
1. [Cleanup and add Styling](7_styling/styling.md)

Finally, there is a simple, but operational website with all the requirements implemented. If you want to run the source, then you will need SWI-Prolog, as some of the libraries used are specific to that implementation. Other Prolog systems have web libraries but they are different, although the concept will be similar I expect.
