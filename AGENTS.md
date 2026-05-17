Repository guidelines
=====================

Project structure & module organization
---------------------------------------

`submark` is a Haskell library, CLI, Docker image, and GitHub Action for
extracting sections from CommonMark/Markdown documents.

 -  *src/Text/CommonMark/Sub.hs*: library API and section-matching logic.
 -  *app/Main.hs*: CLI option parsing and executable behavior.
 -  *test/*: Hspec suite, including quasi-quoted CommonMark fixtures.
 -  *lint/Main.hs*: HLint test target used by Stack.
 -  *action.yaml* and `setup/`: GitHub Action definitions.
 -  *.github/workflows/*: CI, release, Docker, and action test workflows.
 -  *package.yaml* and *stack.yaml*: package metadata and Stack resolver.


Build, test, and development commands
-------------------------------------

 -  `stack setup`: install the configured GHC toolchain if needed.
 -  `stack build`: build the library and `submark` executable.
 -  `stack build --test`: build code and test targets without running tests.
 -  `stack test`: run the Hspec suite and HLint target.
 -  `stack test submark:spec`: run only the functional spec suite.
 -  `stack test submark:hlint`: run only HLint checks.
 -  `stack install`: install the executable locally.
 -  `stack build --flag submark:static --copy-bins`: build a release-style
    static executable where supported.


Coding style & naming conventions
---------------------------------

Follow *.editorconfig*: LF endings, UTF-8, final newline, trimmed trailing
whitespace, 4-space indentation for Haskell, 2-space indentation for YAML, and
an 80-column target. The package enables `-Wall` and incomplete-pattern
warnings; keep new code warning-clean under Stack. Use explicit exported APIs in
`Text.CommonMark.Sub`, descriptive record fields, and module paths matching
*Text/CommonMark/…*.


Testing guidelines
------------------

Tests use Hspec with `hspec-discover`; add behavior specs under
_test/Text/CommonMark/\*Spec.hs_. Prefer examples that exercise parsed
CommonMark nodes and rendered CLI-visible behavior. When adding CLI options,
update README documentation and cover parsing or extraction behavior in tests.
Run `stack test` before opening a pull request.


Commit & pull request guidelines
--------------------------------

Recent commits use short imperative subjects such as `Upgrade GHC to 9.8` and
`Update Dockerfile`. Keep commits focused and explain any release, CI, or action
compatibility impact in the body when relevant. Pull requests should summarize
the change, list validation commands, link related issues, and include README or
CHANGELOG updates when user-facing behavior changes.
