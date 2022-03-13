`dahlia/submark/setup`: GitHub action to install [`submark`]
============================================================

This action installs [`submark`] during GitHub Actions workflow:

~~~ yaml
- uses: dahlia/submark/setup@0.3.0
~~~

It installs the same version of `submark` to the action (the tag after `@`
refers the version) by default.  To explicitly specify the version to install,
use the `submark-version` option:

~~~ yaml
- uses: dahlia/submark/setup@0.3.0
  with:
    submark-version: 0.3.*
~~~

The wildcard in the version number chooses the latest released version.  Also,
`submark-version: 0.*` is equivalent to `submark-version: 0.*.*`,
and `submark-version: *` is equivalent to `submark-version: *.*.*`.  Therefore,
`submark-version: *` means the latest version.

To get the exact version number of the installed `submark` from the later steps,
use the `submark-version` output:

~~~ yaml
- id: setup-submark
  uses: dahlia/submark/setup@0.3.0
  with:
    submark-version: *
- run: |
    echo "Installed submark version: ${{ steps.setup-submark.submark-version }}"
~~~

To add the installed `submark` to the `PATH`, turn off the `add-to-path` option
(which is turned on by default) and use the `submark-path` output:

~~~ yaml
- id: setup-submark
  uses: dahlia/submark/setup@0.3.0
  with:
    add-to-path: false
- run: |
    ${{ steps.setup-submark.submark-path }} \
      --h1 "Version $GITHUB_REF_NAME"
      CHANGES.md
~~~

[`submark`]: ..