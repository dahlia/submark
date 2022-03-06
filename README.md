`submark`: Extract a part from CommonMark/Markdown docs
=======================================================

[![GitHub Actions][gh-actions-badge]][gh-actions]
[![Hackage][hackage-badge]][hackage]

`submark` is a CLI program to extract some particular section from
a given CommonMark/Markdown document.  I use it for myself to extract
the latest version section from the *CHANGELOG.md* file, and then reuse the text
for the corresponding release note on GitHub releases, during automated release
process which is run on CI.

[gh-actions-badge]: https://github.com/dahlia/submark/actions/workflows/build.yaml/badge.svg
[gh-actions]: https://github.com/dahlia/submark/actions/workflows/build.yaml
[hackage-badge]: https://img.shields.io/hackage/v/submark.svg
[hackage]: https://hackage.haskell.org/package/submark


Download & installation
-----------------------

Prebuilt binaries for the following platforms and architectures are available on
[GitHub releases]:

 -  Linux (x86_64)
 -  macOS (x86_64)
 -  Windows (win64)

For other platforms and architectures, you need to build it by yourself.
It's written in Haskell, so you need to install [Haskell Stack] first.
It can be built in the same way other Haskell programs are:

~~~~~~~~ bash
$ stack setup && stack install
~~~~~~~~

[GitHub releases]: https://github.com/dahlia/submark/releases
[Haskell Stack]: https://haskellstack.org/


Usage examples
--------------

The following examples use [John Gruber's original Markdown introduction][1]:

~~~~~~~~ bash
$ wget https://daringfireball.net/projects/markdown/index.text
~~~~~~~~

Extracting an H3 heading *Command-Line*:

~~~~~~~~ bash
$ submark --h3 "Command-Line" index.text
### Command-Line

Use the `--html4tags` command-line switch to produce HTML output from a
Unix-style command line. E.g.:

    % perl Markdown.pl --html4tags foo.text

Type `perldoc Markdown.pl`, or read the POD documentation within the
Markdown.pl source code for more information.
~~~~~~~~

Note that its textual style differ from the original text.
Since ``submark`` internally builds an abstract syntax tree from the given input
text and then render again the result tree, it doesn't maintain trivial styles
(e.g., whitespaces, ATX vs. Setext headings) but only the semantics.

There are options from `--h1` to `--h6`:

~~~~~~~~ bash
$ submark --h2 "Download" index.text
## Download

[Markdown 1.0.1](http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip)
(18 KB) -- 17 Dec 2004
~~~~~~~~

The leading heading can be omitted:

~~~~~~~~ bash
$ submark --h2 "Download" --omit-heading index.text
[Markdown 1.0.1](http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip)
(18 KB) -- 17 Dec 2004
~~~~~~~~

Matching is case sensitive by default, but case can be ignored using the option
`-i`/`--ignore-case`:

~~~~~~~~ bash
$ submark --h2 "DOWNload" index.text

$ submark --h2 "DOWNload" --ignore-case index.text
## Download

[Markdown 1.0.1](http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip)
(18 KB) -- 17 Dec 2004
~~~~~~~~

By Unix convention, `-` means pipe: 

~~~~~~~~ bash
$ submark --h2 "Download" - < index.text
## Download

[Markdown 1.0.1](http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip)
(18 KB) -- 17 Dec 2004
~~~~~~~~

There's `-o`/`--out-file` option as well:

~~~~~~~~ bash
$ submark -o download.md --h2 "Download" index.text
~~~~~~~~

[1]: https://daringfireball.net/projects/markdown/index.text
