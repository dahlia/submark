`submark` changelog
===================

Version 0.3.1
-------------

To be released.

 -  Added `dahlia/submark` action for GitHub Actions.  [[#6]]
 -  Added `dahlia/submark/setup` action for GitHub Actions.  [[#6]]

[#6]: https://github.com/dahlia/submark/issues/6


Version 0.3.0
-------------

Released on March 11, 2022.

 -  Added options from `--h1-regex` to `--h6-regex`.  [[#2]]
 -  Added `Text.CommonMark.Sub.HeadingPattern` type.  [[#2]]
 -  Added `Text.CommonMark.Sub.HeadingTitlePattern` type.  [[#2]]
 -  The signature of `Text.CommonMark.Sub.extractSection` function was changed
    from `Level -> (Text -> Text -> Bool) -> Text -> Node -> Node` to
    `HeadingPattern -> Node -> Node`.  [[#2]]
 -  The signature of `Text.CommonMark.Sub.matchesHeading` function was changed
    from `Level -> (Text -> Text -> Bool) -> Text -> Node -> Bool` to
    `HeadingPattern -> Node -> Bool`.  [[#2]]

[#2]: https://github.com/dahlia/submark/issues/2


Version 0.2.0
-------------

Released on November 6, 2017.

 -  Added `-i`/`--ignore-case` option for case insensitive match.

 -  Wordwrap became turned off by default unless `-c`/`--columns` option is
    present.

 -  The signature of `Text.CommonMark.Sub.extractSection` function was changed
    from `Level -> Text -> Node -> Node` to
    `Level -> (Text -> Text -> Bool) -> Text -> Node -> Node`; the second
    parameter, a predicate for text equality, was added.

 -  The signature of `Text.CommonMark.Sub.matchesHeading` function was changed
    from `Level -> Text -> Node -> Bool` to
    `Level -> (Text -> Text -> Bool) -> Text -> Node -> Bool`; the second
    parameter, a predicate for text equality, was added.


Version 0.1.0
--------------

Initial release.  Released on September 24, 2017.
