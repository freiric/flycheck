Flycheck [![Build Status](https://travis-ci.org/lunaryorn/flycheck.png)](https://travis-ci.org/lunaryorn/flycheck)
========

Flycheck provides on-the-fly syntax checking for GNU Emacs 24.  Essentially it's
“flymake done right” with

- major-mode based checkers (instead of file name patterns),
- simple declarative checker definitions (instead of init functions),
- ready-to-use syntax checkers for a bunch of languages (instead of broken
  checkers using non-existing tools),
- and a clean, concise and understandable implementation (instead of a almost 2k
  line mess of spaghetti code).

And this is how it looks like (the color theme is [solarized-light][]):

![Screenshot of Flycheck in action](https://github.com/lunaryorn/flycheck/raw/master/screenshot.png)


Features
--------

- Automatic syntax check after saving or insertion of new lines
- Error navigation with `next-error` and `previous-error`
- Built-in syntax checkers for:
  - CoffeeScript
  - CSS
  - Emacs Lisp
  - Haml
  - HTML
  - Javascript
  - JSON
  - Lua
  - Perl
  - PHP
  - Python
  - Ruby
  - Sass
  - Shell scripts (Bash, Dash and Zsh)
  - TeX/LaTeX
  - XML
- Easy customization (see `flycheck-checkers`)
- Easy declarative API to define new syntax checkers


Installation
------------

Install the ELPA package from [MELPA][] or [Marmalade][] with `M-x
package-install RET flycheck`.  All dependencies are automatically installed.

Flycheck is written and tested against GNU Emacs 24.2 and newer.  It should work
on Emacs 24.1, too.  GNU Emacs 23 and before, and other flavors of Emacs
(e.g. XEmacs) are *not* supported.

Most checkers have dependencies against external tools that perform the
checking.  Use `C-c ! ?` to see what a checker needs, e.g. `C-c ! ?
python-pylint`.


Usage
-----

This section provides a short introduction to Flycheck.  For complete
documentation, including the declaration of new syntax checkers, refer to the
Info manual of Flycheck.  Open the manual with `C-c ! i`.

Enable `flycheck-mode` in your `init.el` file for all text editing and
programming modes:

```scheme
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'text-mode-hook 'flycheck-mode)
```

You may also enable Flycheck manually after visiting a file with `M-x
flycheck-mode`.

In `flycheck-mode` the buffer is automatically checked on-the-fly.  You can also
check the buffer manually with `C-c ! c`.

A suitable syntax checker is automatically selected from the list of registered
checkers in `flycheck-checkers`.  Use  `C-c ! s` to manually select a specific
syntax checker, or configure the syntax checker per file by setting
`flycheck-checker` as file local variable::

```python
# Local Variables:
# flycheck-checker: python-pylint
# End:
```

Some syntax checkers read configuration files.  Use `M-x customize-group RET
flycheck-config-files` to customize these.  Refer to the section *Configuration*
in the Flycheck info manual.

Errors and warnings from the syntax checker are reported in the mode line,
highlighted in the buffer and indicated with icons in the fringe.  Customize
`flycheck-highlighting-mode` to change the highlighting of errors.  Refer to the
sections *Reporting* and *Mode line* for more information.

**Note:** The default highlighting faces provided GNU Emacs are ill-suited to
highlight errors.  They are too easily overlooked.  Make sure to customize these
faces to add a striking background color or an underlying, or choose a color
theme with reasonable Flycheck faces, for instance the excellent light or dark
[solarized][] themes.

Use `M-g n` and `M-g p` to navigate between errors.  If the point is on an
error, the error message is shown in the echo area after a short delay.  You may
also hover the mouse over a highlighted error to get a tooltip with the error
message.  See *Reporting* for more information.

**Note**: *Visible* compilation buffers take preference over buffers with
Flycheck errors.  This includes buffers from `M-x compile`, `M-x grep` and
generally all buffers with Compilation Mode or Compilation Minor Mode enabled.
If such a buffer is visible `next-error` and `previous-error` will navigate the
errors (or grep results) reported by this buffer instead.  Hide this buffer
(e.g. with `delete-other-windows`) to navigate Flycheck errors again.


Credits
-------

The following people contributed to flycheck:

- [Bozhidar Batsov][bbatsov] provided valuable feedback and refinements, and
  brought Flycheck to a larger user base by adding it to his awesome [Prelude][]
  project.
- [Damon Haley][dhaley] helped to shape and test the PHP CodeSniffer checker.
- [Jimmy Yuen Ho Wong][wyuenho] added the HTML syntax checker and the jshint
  Javascript checker, and did valuable testing and bug fixing.
- [Marian Schubert][maio] added the Perl syntax checker.
- [Martin Grenfell][scrooloose] created the awesome Vim library [syntastic][]
  which inspired this project and many of its checkers.
- [Peter Vasil][ptrv] created the XML and Lua syntax checkers, added unit tests
  and did valuable testing.
- [steckerhalter][] provided the PHP CodeSniffer checker.
- [Steve Purcell][purcell] implemented many checkers, contributed important
  ideas to the design of the checker API and engaged in worthwhile discussion to
  shape this project.


License
-------

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.

See [COPYING][] for details.


[solarized-light]: https://github.com/bbatsov/solarized-emacs
[melpa]: http://melpa.milkbox.net
[marmalade]: http://marmalade-repo.org/
[download]: https://github.com/lunaryorn/flycheck/tags
[dash.el]: https://github.com/magnars/dash.el
[s.el]: https://github.com/magnars/s.el
[solarized]: https://github.com/bbatsov/solarized-emacs
[python]: http://python.org
[checkstyle]: http://checkstyle.sourceforge.net/
[bbatsov]: https://github.com/bbatsov
[prelude]: https://github.com/bbatsov/prelude
[dhaley]: https://github.com/dhaley
[syntastic]: https://github.com/scrooloose/syntastic
[scrooloose]: https://github.com/scrooloose
[purcell]: https://github.com/purcell
[wyuenho]: https://github.com/wyuenho
[maio]: https://github.com/maio
[ptrv]: https://github.com/ptrv
[steckerhalter]: https://github.com/steckerhalter
[copying]: https://github.com/lunaryorn/flycheck/blob/master/COPYING
