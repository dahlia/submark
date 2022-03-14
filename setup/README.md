`dahlia/submark/setup`: GitHub action to install [`submark`]
============================================================

This action installs [`submark`] during GitHub Actions workflow:

~~~ yaml
- uses: dahlia/submark/setup@main
~~~

It installs the latest version of `submark`  by default.  To explicitly specify
the version to install, use the `submark-version` option:

~~~ yaml
- uses: dahlia/submark/setup@main
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
  uses: dahlia/submark/setup@main
  with:
    submark-version: *
- run: |
    echo "Installed submark version: ${{ steps.setup-submark.submark-version }}"
~~~

To prevent the installed `submark` from being added to the `PATH`, turn off
the `add-to-path` option (which is turned on by default) and use
the `submark-path` output instead:

~~~ yaml
- id: setup-submark
  uses: dahlia/submark/setup@main
  with:
    add-to-path: false
- run: |
    ${{ steps.setup-submark.submark-path }} \
      --h1 "Version $GITHUB_REF_NAME"
      CHANGES.md
~~~

[`submark`]: ..


Input parameters
----------------

 -  `submark-version`:  Version of a `submark` binary to install.  Note that
    asterisks can be used to choose the latest version, e.g., `1.2.*`, `1.*`,
    `*`.  (Default: `*`.)
 -  `add-to-path`:  Whether to add the installed `submark` to the `PATH`.
    Turned on by default.  (Default: `true`.)


Output parameters
-----------------

 -  `submark-version`:  Exact version number of the installed `submark`.
 -  `submark-path`:  Absolute path of the installed `submark`.
