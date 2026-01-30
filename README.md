This OxCaml monorepo is intended to be a standalone repository which contains a
fully Dune-buildable set of packages for working with OxCaml. These packages
include not only the Jane Street released libraries, but also community
libraries that have be adapted to work with OxCaml's extensions.

In some cases, these are mechanical changes like porting to Dune from other
build systems, but in other cases we also do larger-scale changes to type
signatures in order to take advantage of OxCaml's extensions like stack
allocation, unboxed types or data-race parallel freedom.

As such, this repository is an unstable moving target, but should provide a
convenient basis for use in open source infrastructure.

## Usage

All you need is the OxCaml compiler (i.e. via an opam switch) and a recent Dune
and then everything else in here will build as a dune switch.

The majority of repos are hidden behind a `(vendored_dirs)` directive which
means that they will only be compiled if there is a dependency on them from a
package in the workspace.

To setup a devcontainer:

```
# initial setup or update
npx @devcontainers/cli up --workspace-folder . --remove-existing-container
# get a shell
npx @devcontainers/cli exec --workspace-folder . bash -l
```

## Custom Code

I'm sticking my own oxcaml code into `avsm/` to leave room for other users
to also contribute. This is a fluid repository so get in touch if you're
using it so I know to not break your usecase.
