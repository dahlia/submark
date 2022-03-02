`subtoml` changelog
===================

Version 0.3.0
-------------

To be released.


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
