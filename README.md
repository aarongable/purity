purity
======

The one, the only, the 500 question purity test... now in Haskell!

The WHAT now?
-------------

The purity test is a  time-honored  tradition of sketchiness and vice. Found at
http://armor.com/tests/500.html, it is a thorough listing of all the things you
may have done, may want to do, or may want to stay the hell as far away from as
you can. Your choice. No one is judging here.

Wait, then what's this?
-----------------------
Have you ever taken the purity test and thought "did I do that?" For those who
wonder "was it a technicality or did I *REALLY* do it?" For those who have said
"I wish I could track this information in git." For those out there that have
said "this is great, but can I get my text editor to do it?" And for those who
have said "great, now can I do it in Haskell?" This is for you.

How it works.
-------------

To get your vice on, simply install ghc and run

    $ runghc purity.hs > test.txt

to get a copy of the test in a nicely formatted plain-text file. You can mark
each question in the test as [D]one, [T]echnicality, [W]ould do, or [ ]
(unknown) by simply placing your response between the square braces. The empty
line between the -----'s and the ======'s is for you to record any extra
information you wish to remember about the question. For example, you might
complete a section of the test like so:

    ===============================================================================
    [D] Spent more time improving your purity-test tracking software than taking the
    purity test
    -------------------------------------------------------------------------------
    It will save me time in the long run, I swear!
    ===============================================================================
    [T] Thought other languages were better than Haskell
    -------------------------------------------------------------------------------
    I was young and foolish! It doesn't really count since I didn't KNOW Haskell at
    the time!
    ===============================================================================
    [W] Attended a vice event after graduating
    -------------------------------------------------------------------------------
    Just waiting for the right time to strike.
    ===============================================================================
    [ ] Seriously tried to learn Vim.
    -------------------------------------------------------------------------------
    Never. Emacs for life!

You can score the test by running purity.hs on it:

    $ ./purity.hs test.txt
    42.0 / 35.4 / 9.8 // 500

The first number is your score according to what you have [D]one. The second
number is your score including [T]echnicalities. The third number is what your
score could be if you did everything you are [W]illing to do. The final number
represents how many questions were found in the file. *Hint:* if it reports fewer
than 500, you messed up your test file.

Extras
------

Finally, because test taking has been reduced to text editing, included is
an Emacs mode to facilitate purity testing. In the repository, you will find
pmode.el. It provides syntax highlighting for the test and keyboard shortcuts.
A list of keyboard shortcuts can be found using C-h m after activating p-mode.
[p-mode marks all questions and section headers as read-only so you can't
accidentally mess the test up.]

So next time someone says "lose a purity point" all you have to do
is pull out your laptop, fire up Emacs,

    M-x occur <ret> \[W\] <ret>

and you'll be ready to go! Easy as pi.

Contributors
------------

* Brad Jensen (author)
* Aaron Gable (host)
