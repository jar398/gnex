GNEX - the GNU emacs port of Kent Pitman's EMACS key bindings known as NEX
=====

(The name NEX was coined by Kent and I'm not sure what it stands for.
I always think of it as "New Emacs eXperimental."  It's an apparent
reference to NE which is what the newest pre-release version of E
(EMACS) was called on ITS, and EX which was the experimental version
of E.  "New" here of course means "new as of 1980".)

If you like GNEX your shell init (.bashrc etc.) might want to look like this:

    GNEX=~/gnex-repo-clone

If you like GNEX your .emacs might want to look like this:

    (load-file "$GNEX/gnex")
    (gnex)
    (load-file "~/.gnex")

