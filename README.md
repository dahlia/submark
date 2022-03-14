`submark`: Extract a part from CommonMark/Markdown docs
=======================================================

[![GitHub Actions][gh-actions-badge]][gh-actions]
[![Hackage][hackage-badge]][hackage]

`submark` is a CLI program (and GitHub action) to extract some particular
section from a given CommonMark/Markdown document.  I use it for myself to
extract the latest version section from the *CHANGELOG.md* file, and then reuse
the text for the corresponding release note on GitHub releases, during automated
release process which is run on CI/CD.

[gh-actions-badge]: https://github.com/dahlia/submark/actions/workflows/build.yaml/badge.svg
[gh-actions]: https://github.com/dahlia/submark/actions/workflows/build.yaml
[hackage-badge]: https://img.shields.io/hackage/v/submark.svg
[hackage]: https://hackage.haskell.org/package/submark


GitHub action: `dahlia/submark`
-------------------------------

Although it's a standalone CLI program, you can use it through a handy GitHub
action `dahlia/submark`:

~~~ yaml
- id: extract-changelog
  uses: dahlia/submark@main
  with:
    input-file: CHANGELOG.md
    heading-level: 2
    heading-title-text: version ${{ github.ref_name }}
    ignore-case: true
    omit-heading: true

# The output-file refers to the path of the temporary file which contains
# the only extracted part:
- run: cat ${{ steps.extract-changelog.output.output-file }}

# The output-text contains the text of the extracted part.
- run: echo $CHANGELOG
  env:
    CHANGELOG: ${{ steps.extract-changelog.output.output-text }}
~~~

### Input parameters

 -  `input-text`:  The input CommonMark/Markdown text.  Mutually exlclusive with
    the `input-file` parameter.
 -  `input-file`:  The input CommonMark/Markdown file path.  Mutually exlclusive
    with the `input-text` parameter.
 -  `heading-level`:  The heading level of the section to extract.
 -  `heading-title-text`:  Extract the section with the exact this
    `heading-title-text` (and the `heading-level`).  Note that it tries to match
    to the heading title with no markup, which means `heading-title-text:
    "foo bar"` matches to both `# foo bar` and `# _foo_ **bar**`.  Mutually
    exclusive with the `heading-title-regex` parameter.
 -  `heading-title-regex`:  Similar to the `heading-title-text` parameter except
    that it takes a regular expression.  Note that it tries to match to
    the heading title with no markup, which means `heading-title-regex:
    "fo+ ba[rz]"` matches to both `# foo bar` and `# _foooo_ **baz**`.
    Mutually exclusive with the `heading-title-text` parameter.
 -  `ignore-case`:  Ignore case distinctions.  (Default: `false`.)
 -  `omit-heading`:  Omit a leading heading.  (Default: `false`.)
 -  `columns`:  Limit the maximum characters per line of the output.
    No limit by default.

### Output parameters

 -  `output-text`:  The text of the extracted part.
 -  `output-file`:  The path to the temporary file which contains the only
    extracted part.

Download & installation
-----------------------

First of all, if you need to manually invoke `submark` on GitHub Actions,
[`dahlia/submark/setup`](./setup/) action is the easiest way to install it.

On the other CI/CD products, use the officially distributed executables.
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

Options from `--h1-regex` to `--h6-regex` take a regular expression instead of
an exact heading text:

~~~~~~~~ bash
$ submark --h2-regex "^(Down|Up)load$" index.text
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


Software license
----------------

This software is distributed under [GPL 3].

[GPL 3]: https://www.gnu.org/licenses/gpl-3.0.html
