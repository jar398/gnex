# ![GNEX: a customized GNU Emacs editing environment ](/assets/Gnex_banner.png)

## Overview

**GNEX** is [Jonathan Rees](https://github.com/netsettler)'s
[GNU Emacs](https://en.wikipedia.org/wiki/GNU_Emacs) port of
[Kent Pitman](https://github.com/netsettler)'s 
[EMACS](https://en.wikipedia.org/wiki/Emacs) key bindings,
mode line, and other editing support, collectively known originally as **NEX**.

## History

The name NEX was coined by @netsettler or @BobKerns, neither is sure
which, somewhere back around 1980. At the time, "NE" was the short name
for "the newest stable version of Emacs", 
and the added "X" meant that there were a set of extensions 
that Kent and Bob used. Of course, "newest stable"
here means "newest as of 1980" and even what's stable changes with time.
Back then, Emacs was still implemented in
[TECO](https://en.wikipedia.org/wiki/TECO_%28text_editor%29), 
for example.

The equivalent extensions were later rewritten in 
[emacs-lisp](https://en.wikipedia.org/wiki/Emacs_Lisp) 
for use
in [GNU Emacs](https://en.wikipedia.org/wiki/GNU_Emacs) by @jar398, who took over maintenance of what is now called
"GNEX".

## Features

Some useful features of the GNEX environment are shown here.
This is probably not an exhaustive list.

 * Alternate GNEX key bindings. There are many of these, but notable ones are:
   * **Space** is `self-insert-or-scroll-up`
   * **Delete** is `backward`
   * **Control-.** is `tags-loop-continue`
   * **Control-\\** is `character-search-forward` (as is **Control-s**)
   * **Control-%** is `tags-search`
   * **Control-h** is `help-command`
   * **Control-i** (and **Control-Tab**) is `indent-differently`.
   * **Control-l** is `force-redisplay`
   * **Control-r** is `character-search-backward`
   * **Control-s** is `character-search-forward`
 * Alternate dispatch characters to select from GNEX-extended command tables:
   * **Control-w** is `nex-meta-prefix`, using `nex-meta-map`.
   * **Control-^** uses `nex-control-prefix`, using `nex-control-map`.
   * **Control-\[** (a.k.a. **Esc**) uses `nex-meta-map` directly:
     * **Meta-i** is `indent-relative`.
     * **Meta-s** is `center-line`.
     * **Meta-Space** is `just-one-space`.
     * **Meta-/** is `dabbrev-expand`.
     * **Meta-|** is `draw-vertical-line` (useful in Picture Mode).
     * **Meta-,** is `mini-find-tag`.
     * **Meta-\]** is `forward-paragraph`.
     * **Meta-\[** is `backward-paragraph`.
   * **Control-z** is `nex-control-meta-prefix`, using `nex-control-meta-map`:
     * **Control-Meta-l** is `switch-to-previous-buffer`.
     * **Control-Meta-q** is `scheme-indent-sexp`.
     * **Control-Meta-Del** is `backward-kill-sexp`.
     * **Control-Meta-;** is `kill-comment`.
     * **Control-Meta-%** is `tags-query-replace`.
   * **Control-x** is `nex-control-x-prefix`, using `nex-control-x-map`:
     * **Control-x i** is `info`.
     * **Control-x r** is `read-mail`.
     * **Control-x #** is `insert-counter` (useful in keyboard macros).
     * **Control-x Control-b** is `electric-buffer-list`.
     * **Control-x Control-c** is `suspend-emacs`.
     * **Control-x Control-k** is `kill-region`.
     * **Control-x Control-y** is `insert-file`.
     * **Control-x Control-\\** is `save-buffer` (like **Control-X Control-s**).
 * Attempted fixes for various known bugs
 * Custom indentations for a number of lisp special forms.
 * Use of the **modlin** library sets up a different look for the mode line.

## Setup

### Setup in Bash init files

If you like GNEX your shell init (`.bashrc` etc.) might want to look like this:

    GNEX=~/gnex-repo-clone

### Setup in Emacs init files

If you like GNEX your .emacs might want to look like this:

    (load-file "$GNEX/gnex")
    (gnex)
    (load-file "~/.gnex")
