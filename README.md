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

# Packages

## OxCaml Extensions

Packages using OxCaml features (unboxed types, stack allocation, modes, etc.)

- **bytesrw (0.3.0)**: Port from ocamlbuild/topkg to dune build system with OxCaml optimizations using local parameters, mutable locals, unboxed types, and zero_alloc annotations
  <details><summary>Details</summary>

  ## Build System Migration (dune-port)
  - Removed ocamlbuild files: `_tags`, `myocamlbuild.ml`, `pkg/pkg.ml`, `pkg/META`, `opam`
  - Removed `.mllib` and `.clib` files for all sub-libraries
  - Added `dune-project` and `dune` files throughout
  - Added `config/discover.ml` for each C library binding (blake3, crypto, md, tls, xxhash, zlib, zstd) using dune-configurator for pkg-config detection
  - Added `doc/dune` for documentation
  
  ## OxCaml Optimizations (oxcaml)
  - **Base.Bytes integration**: Added `module Local_bytes = Base.Bytes` for local-aware byte operations
  - **Local parameters**: Functions like `is_eod`, `first`, `last`, `length`, `copy`, `compare`, `equal`, `to_bytes`, `to_string`, `to_bigbytes`, `add_to_buffer`, `output_to_out_channel`, `pp_meta` now accept `(s @ local)` parameters
  - **Mutable locals**: Replaced `ref` cells with `let mutable` for loop variables throughout (e.g., `let mutable i = 0` instead of `let i = ref 0`)
  - **Zero-alloc annotations**: Added `[@zero_alloc]` and `[@inline]` attributes to hot-path functions
  - **Unboxed char patterns**: Added `uchar_utf_8_byte_decode_length_unboxed` using `char#` unboxed type with patterns like `#'\x00'`
  - **Slice.make_local**: New function with `exclave_` for stack allocation
  - **Optimized compare**: Rewrote `Slice.compare` using mutable locals instead of refs
  
  ## Code Improvements
  - Rewrote recursive functions as while loops with mutable state (skip, add_to_buffer, write_bytes, output_to_out_channel, etc.)
  - Optimized `of_binary_string` with lookup table and direct byte manipulation
  - Simplified `hex_value` functions with inline annotations
  - Improved `ilog2` using mutable locals
  
  ## Interface Changes (bytesrw.mli)
  - Added `[@@zero_alloc]` attributes to accessor functions
  - Changed parameter modes to `@ local` for slice accessors and converters
  </details>
- **js_of_ocaml**: Add OxCaml unboxed types support including Float32, Null, and mixed blocks, plus OCaml 5.x API compatibility
  <details><summary>Details</summary>

  - **Float32 support**: Add `Float32` constant type for unboxed float32 values with proper parsing, printing, and code generation
  - **Null value support**: Add `Null` constant type with `%is_null` primitive for null-aware code
  - **Unboxed type constants**: Handle `Const_unboxed_float`, `Const_unboxed_float32`, `Const_unboxed_int32`, `Const_unboxed_int64`, `Const_unboxed_nativeint`
  - **Untagged integer types**: Support `Const_int8`, `Const_int16`, `Const_untagged_int8`, `Const_untagged_int16`, `Const_untagged_char`
  - **Mixed blocks**: Handle `Const_mixed_block`, `Const_float_block`, and `MAKE_FAUX_MIXEDBLOCK` opcode
  - **Async exception handling**: Wrap main entry points with `Sys.with_async_exns`
  - **OCaml 5.x API updates**: Use `Cmo_format.compilation_unit_descr`, `Compilation_unit` module APIs, `Ident.is_global`, `Import_info.t`
  - **Runtime improvements**: Float32Array-based float32 operations, `caml_create_file` fallback for file system
  - **Reserved words**: Add `Float32Array` to JavaScript reserved words
  </details>
- **notty-community (0.2.4)**: OxCaml portability annotations, new terminal features (cursor styles, hyperlinks, title control), and multi-domain safety improvements
  <details><summary>Details</summary>

  - **OxCaml Portability**: Added `@@ portable` annotations to external C functions and signal handlers for multi-domain safety
  - **New Features**:
    - Cursor style support (`Bar`, `Block`, `Underline` with blinking variants, `Default`)
    - Hyperlink support via `A.href ~url` attribute
    - Terminal title control (`set_title`, `save_title`, `restore_title`)
    - Exposed `Tty_width_hint` module for Unicode width calculations
    - `default` color value added
    - Ctrl+Backspace now distinguished from regular Backspace
  - **Multi-domain Safety**:
    - Replaced `Hashtbl` with `Portable_lockfree_htbl` for signal handler registry
    - Changed `winched` mutable field to use `[@atomic]` annotation with `Portable.Atomic.Loc` operations
    - Used `Sys.Safe.signal` instead of `Sys.signal` for signal handling
    - Added `Base.Obj.magic_portable` for Lwt compatibility
  - **Build Changes**:
    - Added dependencies: `base`, `portable`, `portable_lockfree_htbl`
    - Added `ppx_jane` preprocessor for `[@@deriving equal]` and `[%string]`
  - **Rendering Improvements**:
    - Line-based diffing to reduce terminal output when only parts of screen change
    - Fixed newline escape code (`\x1b[E` for cursor_nextline, `\n` for newline)
    - Do not send clear-eol on full lines to fix terminal emulator compatibility
  - **Patch Files**: Multiple `.patch` files document individual feature additions
  </details>
- **ocaml-lsp-server (1.19.0+ox)**: Jane Street port with Async scheduler, new code actions, and OxCaml/merlin-lib compatibility
  <details><summary>Details</summary>

  - **Async Integration**: Replaced `lev`/`lev_fiber` event loop with Jane Street `async`/`core` runtime
    - New `fiber-async` library providing `Fiber`-to-`Deferred` interop
    - New `lev-fiber-async` library implementing `Lev_fiber.S` interface over Async
    - Main binary rewritten to use `Command.async` instead of `Arg` parsing
  - **New Code Actions**: Added several code actions not in upstream:
    - `action_add_rec`: Add missing `rec` keyword quick fix
    - `action_combine_cases`: Combine match cases into one line
    - `action_construct`: Construct values from typed holes
    - `action_destruct`/`action_destruct_line`: Destruct/enumerate cases
    - `action_extract`: Extract local bindings
    - `action_inferred_intf`: Generate interface files
    - `action_inline`: Inline let bindings
    - `action_mark_remove_unused`: Mark/remove unused values
    - `action_open_related`: Open related modules
    - `action_refactor_open`: Refactor module opens
    - `action_type_annotate`/`action_remove_type_annotation`: Type annotation actions
  - **Dependency Changes**:
    - Uses `merlin-lib (= "5.2.1-502+ox")` instead of `merlin-lib (>= 5.6)`
    - Uses `odoc-parser (= "3.1.0+ox")` instead of standard `odoc_parser`
    - Added `core`, `core_unix`, `async`, `re2`, `cmarkit` dependencies
    - Uses `yojson (>= 2.0.0) (< 3.0.0)` with constrained version range
    - Version changed from `1.25.0` to `1.19.0+ox`
  - **OxCaml AST Compatibility**: Handles OxCaml-specific AST nodes:
    - `Ppat_record_unboxed_product`, `Ppat_unboxed_tuple`
    - `Pexp_record_unboxed_product`, `Pexp_unboxed_tuple`, `Pexp_unboxed_field`
    - `Ptyp_unboxed_tuple`, `Ptype_record_unboxed_product`
    - `Pconst_unboxed_integer`
  - **Build System**: Uses Jane Street ppx preprocessors (`ppx_let`, `ppx_log`, `ppx_sexp_conv`, `ppx_jane`)
  - **Version Pinning**: OCamlformat pinned to `0.27.0` (vs `0.28.1` upstream)
  </details>
- **ppxlib**: Extensive updates to support OxCaml extended AST with unboxed types, modes, jkinds, comprehensions, and metaprogramming features.
  <details><summary>Details</summary>

  - **Unboxed types support**: Added `Pconst_unboxed_integer`, `Pconst_unboxed_float`, `Pconst_untagged_char` constants; `Ptyp_unboxed_tuple`, `Ppat_unboxed_tuple`, `Pexp_unboxed_tuple` for unboxed tuple types/patterns/expressions; `Ptype_record_unboxed_product`, `Ppat_record_unboxed_product`, `Pexp_record_unboxed_product` for unboxed records; `Pexp_unboxed_field` for unboxed field access; `index_kind` for unboxed integer array indexing
  - **Mode annotations**: Added `mode`, `modes`, `modality`, `modalities` types throughout the AST; extended `Ptyp_arrow` with mode annotations; added `mode_annotations` and `ret_mode_annotations` to function constraints; added modes to pattern/expression constraints, functor parameters, module types
  - **Jkind annotations**: Added `jkind_annotation` type with variants for default, abbreviation, mod, with, kind_of, and product; extended `Ptyp_any`, `Ptyp_var`, `Ptyp_alias`, `Ptyp_poly` with optional jkind annotations; added `ptype_jkind_annotation` to type declarations; jkind support in constructor declarations and extension constructors
  - **Labeled tuples**: Extended tuple types/patterns/expressions with optional string labels: `(string option * core_type) list` format
  - **Comprehensions**: Added `comprehension_expression`, `comprehension`, `comprehension_clause`, `comprehension_iterator`, `comprehension_clause_binding` types for list/array comprehensions; `Pexp_comprehension` expression variant
  - **New expression forms**: `Pexp_stack` for stack allocation, `Pexp_overwrite` for overwriting, `Pexp_quote`/`Pexp_splice` for runtime metaprogramming, `Pexp_hole` for holes, `Pexp_idx` for indexed access with unboxed fields
  - **Function representation change**: Replaced `Pexp_fun` with unified `Pexp_function` taking `function_param list * function_constraint * function_body`; added `function_param_desc` with `Pparam_val` and `Pparam_newtype`
  - **Mutable let bindings**: Extended `Pexp_let` with `mutable_flag` parameter
  - **Module system extensions**: Added `include_kind` (Structure/Functor), `Pmty_strengthen` for module type strengthening, `Pmod_instance` for module instances, `Psig_kind_abbrev`/`Pstr_kind_abbrev` for kind abbreviations
  - **Signature changes**: `signature` now a record with `psg_modalities`, `psg_items`, `psg_loc`
  - **AST version**: Added `ast_999.ml` for the OxCaml extended AST version with migration functions `migrate_500_999.ml` and `migrate_999_500.ml`
  - **Other changes**: `Pexp_setinstvar` renamed to `Pexp_setvar`; arrays now carry `mutable_flag`; open patterns in tuples; modalities in include descriptions, module declarations, value descriptions, label declarations, constructor arguments
  </details>
- **re**: Add OxCaml portable/mode annotations for thread-safe concurrent regex matching
  <details><summary>Details</summary>

  - **OxCaml type annotations**: Added `@@ portable` module annotations and jkind annotations (`type t : immutable_data`, `type t : mutable_data`, `type t : immediate`) throughout the codebase to enable data-race-free parallelism
  - **Mode crossing attributes**: Added `[@@unsafe_allow_any_mode_crossing]` to types with mutable fields that use double-checked locking for thread safety (e.g., `State.t`, `Marks.t`, `re`)
  - **Portable data structures**: Changed `Hashtbl.Make` to `Hashtbl.MakePortable`, `Set.Make` to `Set.MakePortable`, `Map.Make` to `Map.MakePortable` for thread-safe concurrent access
  - **Iarray usage**: Added new `Iarray` module for immutable arrays, converted `Mark_infos.t` from mutable `int array` to immutable `int iarray`
  - **Dense_map portability**: Rewrote to use `Iarray` for portable function memoization
  - **Runtime-conditional Mutex**: Added a `Mutex` module that is a no-op on runtime4 (for js_of_ocaml compatibility) but real on runtime5
  - **Obj.magic_uncontended**: Used `Stdlib.Obj.magic_uncontended` for initialization of transition tables to satisfy contention checking
  - **Bug fix for exec_partial**: Fixed `exec_partial` to correctly return `Full` instead of `Partial` when a pattern fully matches at end of input without groups
  - **Test updates**: Updated concurrency tests to remove TSAN suppressions (no longer needed with proper mode annotations), added new tests for exec_partial consistency
  - **Build changes**: Downgraded dune lang from 3.15 to 3.12, added `seq` dependency, removed `js_of_ocaml` test dependency, enabled benchmarks in CI
  - **Perl parser refactoring**: Inlined character class definitions to avoid portability issues with module-level values
  </details>
- **uutf (1.0.3)**: Add dune build support and OxCaml portability/locality annotations for data-race-free parallelism.
  <details><summary>Details</summary>

  - **Dune port**: Added dune-project, src/dune, doc/dune, test/dune; removed B0.ml, BRZO, _tags, pkg/META, pkg/pkg.ml, src/uutf.mllib
  - **OxCaml portability**: Added `@@ portable` annotation to uutf.mli header
  - **OxCaml contended arrays**: Added `magic_uncontended` external function to safely access the immutable `utf_8_len` array in a portable/contended-safe manner
  - **OxCaml locality**: Changed `'a folder` type to use `local_` function types (`'a -> local_ (int -> [...] -> 'a)`) for stack allocation optimization
  - **OxCaml compatibility**: Replaced `Bytes.empty` with `Bytes.create 0` in multiple locations (eoi, src, decoder, encoder functions) for OxCaml compatibility
  - **Test rename**: Renamed test/test.ml to test/test_uutf.ml for dune conventions
  </details>
- **zarith (1.14+ox)**: Add dune build support, OxCaml portability annotations, and Windows/macOS compatibility improvements.
  <details><summary>Details</summary>

  - **Dune build support**: Added `dune`, `dune-project`, and `tests/dune` files for building with dune, including rules to run configure and extract CFLAGS/LIBS
  - **OxCaml annotations**: 
    - Added `@@ portable` annotations to all external C functions in `z.ml` and `z.mli`
    - Added `@@portable` module annotation to `z.mli` and `q.mli`
    - Changed type `t` to `t : immutable_data` in both Z and Q modules
    - Added `global_` annotations to Q.t record fields
    - Added `local_` parameter annotations for input strings, int32, int64, nativeint, and float values
    - Added `compare__local` and `equal__local` variants for Q module
    - Used `Callback.Safe.register_exception` instead of `Callback.register_exception`
  - **Configure improvements**:
    - Added Windows (MSVC, mingw) support with proper suffix detection (`.obj`, `.lib`, `.dll`)
    - Outputs `Makefile.config` instead of `Makefile` (separating config from build rules)
    - Added `LIBSUFFIX`, `DLLSUFFIX`, `EXESUFFIX`, `WORDSIZE`, `STDLIBDIR` variables
    - Bumped minimum OCaml version from 4.04 to 4.07
  - **Platform discovery**: Added `discover.ml` helper to auto-detect GMP include/lib paths on macOS (Homebrew arm64), FreeBSD, OpenBSD
  - **Build fixes**: Updated tests/Makefile to use variables from Makefile.config instead of hardcoding paths
  - **Minor fixes**: Typo fix in README ("intergers" → "integers"), optimized `Q.abs` to avoid allocation when number is already non-negative
  - **Removed**: `project.mak` file (replaced by dune build rules)
  </details>

## Dune Ports

Packages with dune build support added

- **astring (0.8.5)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Add `dune-project` file with dune 3.20 language version
  - Add `src/dune` with library definitions for `astring` and `astring.top`
  - Add `test/dune` with test executables and runtest rules
  - Add `doc/dune` for documentation
  - Move `opam` to `astring.opam` (standard dune naming)
  - Remove ocamlbuild files: `_tags`, `pkg/META`, `pkg/pkg.ml`
  - Remove `.mllib` files: `src/astring.mllib`, `src/astring_top.mllib`
  </details>
- **bos (0.2.1)**: Add dune build support, replacing ocamlbuild/topkg build system.
  <details><summary>Details</summary>

  - Add `dune-project` file with lang dune 3.20
  - Add `src/dune` with three libraries: `bos`, `bos.setup`, and `bos.top`
  - Add `test/dune` with test executables and runtest rule
  - Add `doc/dune` for documentation
  - Move `opam` file to `bos.opam` (standard dune location)
  - Remove ocamlbuild files: `_tags`, `B0.ml`, `pkg/pkg.ml`, `pkg/META`
  - Remove `.mllib` files: `bos.mllib`, `bos_setup.mllib`, `bos_top.mllib`
  </details>
- **brr (0.0.7)**: Replace ocamlbuild/topkg build system with dune while preserving the same library structure.
  <details><summary>Details</summary>

  - Remove ocamlbuild files: `B0.ml`, `BRZO`, `_tags`, `myocamlbuild.ml`, `pkg/pkg.ml`, `pkg/META`, `src/brr.mllib`
  - Add `dune-project` file with lang dune 3.20
  - Add `src/dune` for main brr library with modules and js_of_ocaml-compiler.runtime dependency
  - Add `src/ocaml_poke/dune` for brr.ocaml_poke sublibrary
  - Add `src/ocaml_poke_ui/dune` for brr.ocaml_poke_ui sublibrary
  - Add `src/poke/dune` for brr.poke sublibrary with js_of_ocaml-toplevel dependency
  - Add `src/poked/dune` for brr.poked sublibrary
  - Add `src/console/dune` with install stanza for console assets (devtools.html, devtools.js, etc.)
  - Add `doc/dune` for documentation and odoc-assets installation
  - Move `opam` to `brr.opam` (standard dune naming convention)
  </details>
- **cmarkit (0.3.0)**: Port from ocamlbuild/topkg/b0 to dune build system
  <details><summary>Details</summary>

  - Add `dune-project` file with dune 3.20 and package name
  - Add `src/dune` with library definition (unwrapped, public_name cmarkit)
  - Add `src/tool/dune` for the optional cmarkit executable (requires cmdliner)
  - Add `doc/dune` for documentation
  - Move `opam` file to standard `cmarkit.opam` location
  - Remove B0.ml build configuration
  - Remove pkg/pkg.ml topkg build script
  - Remove pkg/META file (dune generates this)
  - Remove src/cmarkit.mllib (replaced by dune library definition)
  - Remove _tags and BRZO ocamlbuild configuration files
  </details>
- **fmt (0.10.0)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Added `dune-project` file with lang dune 3.20
  - Added `dune` files for all libraries: `src/dune` (fmt), `src/cli/dune` (fmt.cli), `src/tty/dune` (fmt.tty), `src/top/dune` (fmt.top)
  - Added `test/dune` for test executable with runtest alias
  - Added `doc/dune` for documentation
  - Moved `opam` file to `fmt.opam` (standard dune naming)
  - Removed ocamlbuild artifacts: `B0.ml`, `_tags`, `pkg/META`, `pkg/pkg.ml`, `src/fmt.mllib`
  </details>
- **fpath (0.7.3)**: Add dune build support replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Added `dune-project` file with dune 3.20 language version
  - Added `src/dune` defining the main `fpath` library with astring dependency
  - Added `src/top/dune` for the toplevel support library (`fpath.top`)
  - Added `test/dune` with test executable using b0.testing
  - Added `doc/dune` for documentation
  - Renamed `opam` to `fpath.opam` (standard dune convention)
  - Removed ocamlbuild files: `_tags`, `pkg/pkg.ml`, `pkg/META`
  - Removed `.mllib` files: `src/fpath.mllib`, `src/fpath_top.mllib`
  </details>
- **hmap (0.8.1)**: Port from ocamlbuild/topkg to dune build system
  <details><summary>Details</summary>

  - Remove ocamlbuild files: `_tags`, `pkg/pkg.ml`, `pkg/META`, `src/hmap.mllib`
  - Remove legacy odoc files: `doc/api.odocl`, `doc/dev.odocl`
  - Remove old opam 1.2 format `opam` file
  - Add `dune-project` with lang dune 3.20
  - Add `src/dune` library definition (unwrapped)
  - Add `test/dune` with executable and runtest alias
  - Add `hmap.opam` in opam 2.0 format with dune build commands
  - Update OCaml version requirement from 4.02.0 to 4.08.0
  </details>
- **htmlit (0.2.0)**: Add dune build support, replacing ocamlbuild/topkg build system.
  <details><summary>Details</summary>

  - Add `dune-project` with lang dune 3.21 and package definition
  - Add root `dune` file with compiler flags matching ocamlbuild settings (-g -bin-annot -safe-string)
  - Add `src/dune` defining the htmlit library with `(wrapped false)`
  - Add `test/dune` for the example executable
  - Replace `opam` with `htmlit.opam`, updating build dependencies from ocamlfind/ocamlbuild/topkg to dune
  </details>
- **jsonm (1.0.2)**: Add dune build support, removing ocamlbuild/topkg infrastructure.
  <details><summary>Details</summary>

  - Added `dune-project` with lang dune 3.20
  - Added `src/dune` library definition with uutf dependency
  - Added `test/dune` with executables (jsontrip, test, examples, jtree) and runtest rule
  - Added `doc/dune` for documentation
  - Renamed `opam` to `jsonm.opam` (standard dune convention)
  - Removed ocamlbuild files: `_tags`, `src/jsonm.mllib`, `pkg/pkg.ml`, `pkg/META`
  - Removed B0 build system files: `B0.ml`, `BRZO`
  </details>
- **jsont (0.2.0)**: Port from ocamlbuild/topkg to dune build system
  <details><summary>Details</summary>

  - Remove B0.ml build description file
  - Remove _tags ocamlbuild configuration
  - Remove pkg/META and pkg/pkg.ml topkg packaging files
  - Remove .mllib files (jsont.mllib, jsont_brr.mllib, jsont_bytesrw.mllib)
  - Add dune-project file with lang dune 3.20
  - Add dune files for main library (src/dune), brr sublib (src/brr/dune), bytesrw sublib (src/bytesrw/dune)
  - Add doc/dune for documentation
  - Update jsont.opam to use dune build commands instead of topkg
  - Remove cmdliner from depopts (was only used for the jsont tool which is not included)
  </details>
- **logs (0.9.0)**: Add dune build support for logs and all sub-libraries
  <details><summary>Details</summary>

  - Add `dune-project` file with dune 3.20 and package name
  - Add `dune` files for all libraries:
    - `src/dune` for the core `logs` library
    - `src/fmt/dune` for `logs.fmt` (with install stanza for `logs_fmt_top_init.ml`)
    - `src/browser/dune` for `logs.browser` (optional, depends on js_of_ocaml-compiler.runtime)
    - `src/cli/dune` for `logs.cli` (depends on cmdliner)
    - `src/lwt/dune` for `logs.lwt` (optional, depends on lwt)
    - `src/threaded/dune` for `logs.threaded` (optional, depends on threads)
    - `src/top/dune` for `logs.top` (with install stanza for `logs_top_init.ml`)
  - Add `doc/dune` for documentation
  - Add `logs.opam` file (moved from `opam`)
  - Remove legacy build files: `B0.ml`, `BRZO`, `_tags`, `pkg/META`, `pkg/pkg.ml`, `src/logs.mllib`
  </details>
- **mtime (2.1.0)**: Port from ocamlbuild/topkg to dune build system with reorganized directory structure.
  <details><summary>Details</summary>

  - Removed ocamlbuild files: `B0.ml`, `BRZO`, `_tags`, `myocamlbuild.ml`, `pkg/pkg.ml`, `pkg/META`, `.mllib`, `.clib` files
  - Added dune build files: `dune-project`, `src/dune`, `src/top/dune`, `src-clock/dune`, `test/dune`, `doc/dune`
  - Renamed opam file from `opam` to `mtime.opam`
  - Reorganized clock library sources from `src/clock/` to `src-clock/` directory
  - Added `Mtime_clock.mli` interface files to both `src/` and `src-clock/` directories
  - Created virtual library `mtime.clock` in `src/dune` with actual implementation `mtime.clock.os` in `src-clock/`
  - Tests marked as optional due to b0 library dependency not being available
  </details>
- **ocamlfind (1.9.8.git)**: Add dune build support with OxCaml compatibility fix for topfind type annotations
  <details><summary>Details</summary>

  - Add `dune-project` and dune build files for findlib library and ocamlfind executable
  - Add `findlib.opam` and `ocamlfind.opam` for dune-based package management
  - Add `tools/discover.ml` for dune-configurator-based configuration (replaces autoconf)
  - Add `tools/extract_args/` executable for extracting OCaml compiler arguments
  - Add META files for standard library packages (bigarray, bytes, compiler-libs, dynlink, raw_spacetime, stdlib, str, threads, unix, runtime_events)
  - Add toplevel scripts `topfind_rd0.p` and `topfind_rd1.p` with native toplevel support
  - Fix in `topfind.ml.in`: change `ignore` to explicit eta-expanded function `fun _ -> ()` for OxCaml compatibility (avoids type inference issues with `ignore`'s polymorphic type)
  </details>
- **ptime (1.2.0)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Add `dune-project` file with dune 3.20 language version
  - Add `src/dune` for main ptime library with `ptime_top_init.ml` install rule
  - Add `src/clock/dune` for ptime.clock library with C stubs, js_of_ocaml runtime, and deprecated ptime.clock.os alias
  - Add `src/top/dune` for ptime.top library with compiler-libs.toplevel dependency
  - Add `doc/dune` for documentation
  - Move `opam` to `ptime.opam` (dune naming convention)
  - Remove ocamlbuild files: `B0.ml`, `BRZO`, `_tags`, `myocamlbuild.ml`, `pkg/pkg.ml`, `pkg/META`
  - Remove `src/ptime.mllib`
  </details>
- **react (1.2.2)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Add `dune-project` with dune 3.20 and package name
  - Add `src/dune` defining the `react` library and `react.top` sublibrary
  - Add `test/dune` with test executables (test, clock, breakout) and runtest rule
  - Add `doc/dune` for documentation
  - Move opam file from `opam` to `react.opam` (dune convention)
  - Remove ocamlbuild files: `_tags`, `src/react.mllib`, `src/react_top.mllib`
  - Remove topkg packaging: `pkg/pkg.ml`, `pkg/META`
  - Remove B0 build configuration: `B0.ml`, `BRZO`
  </details>
- **rresult (0.7.0)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Added `dune-project` with lang dune 3.20
  - Added `src/dune` defining rresult and rresult.top libraries
  - Added `test/dune` with executable and runtest alias
  - Added `doc/dune` for documentation
  - Renamed `opam` to `rresult.opam` (dune convention)
  - Removed ocamlbuild files: `_tags`, `src/rresult.mllib`, `src/rresult_top.mllib`
  - Removed topkg files: `pkg/pkg.ml`, `pkg/META`
  - Removed B0 build file: `B0.ml`
  </details>
- **uucp (16.0.0)**: Add dune build support while removing ocamlbuild/topkg infrastructure.
  <details><summary>Details</summary>

  - Added `dune-project` file with dune 3.20 and package name
  - Added `src/dune` defining the uucp library with `(wrapped false)`
  - Added `test/dune` with executables for ucharinfo (public, optional), link_test, and perf
  - Added `doc/dune` for documentation
  - Added `uucp.opam` (moved from `opam` file at root)
  - Removed `B0.ml` (b0 build system configuration)
  - Removed `BRZO` configuration file
  - Removed `_tags` (ocamlbuild tags)
  - Removed `pkg/pkg.ml` (topkg package description)
  - Removed `pkg/META` (findlib metadata, now generated by dune)
  - Removed `src/uucp.mllib` (ocamlbuild library description)
  </details>
- **uunf**: Port from ocamlbuild/topkg to dune build system
  <details><summary>Details</summary>

  - Add `dune-project` file with dune 3.20 language version
  - Add `src/dune` library definition (public_name uunf, wrapped false, suppresses warning 32)
  - Replace `opam` file with `uunf.opam` using dune build command
  - Update homepage to dune-universe fork in opam metadata
  - Remove ocamlbuild, topkg, and uucd dependencies from opam
  - Replace hardcoded version strings with `%%VERSION%%` placeholders in README.md, doc/index.mld, pkg/META, and test/unftrip.ml
  - Add development files: `.gitignore`, `.merlin`, `.ocp-indent`
  </details>
- **uuseg (16.0.0)**: Add dune build support while removing ocamlbuild/topkg build system files
  <details><summary>Details</summary>

  - Added `dune-project` file with dune 3.20 language version
  - Added `src/dune` defining the uuseg library with uucp dependency and `wrapped false`
  - Added `test/dune` with executables for usegtrip and test_uuseg (marked optional)
  - Added `doc/dune` for documentation
  - Renamed `opam` to `uuseg.opam` (dune convention)
  - Removed ocamlbuild files: `_tags`, `src/uuseg.mllib`
  - Removed topkg files: `pkg/pkg.ml`, `pkg/META`
  - Removed B0 build files: `B0.ml`, `BRZO`
  - Tests are disabled in dune (commented out runtest rule) as they require b0.testing library
  </details>
- **xmlm (1.4.0)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Added `dune-project` file with dune 3.20 and package name
  - Added `src/dune` defining the xmlm library
  - Added `test/dune` with executables (xmltrip, test, examples, test_tree) and runtest rules
  - Added `doc/dune` for documentation
  - Renamed `opam` to `xmlm.opam` (dune convention)
  - Removed ocamlbuild files: `_tags`, `src/xmlm.mllib`, `pkg/META`, `pkg/pkg.ml`
  - Removed B0 build file: `B0.ml`
  </details>

## Compatibility

Packages with monorepo compatibility changes

- **alcotest (1.9.0)**: Fix partial application to use explicit parameter for compatibility with stricter typing
  <details><summary>Details</summary>

  - Modified `length_utf8` function in `alcotest_stdlib_ext.ml` to use explicit parameter `t` instead of point-free style
  - Changed from `let length_utf8 = Uutf.String.fold_utf_8 ...` to `let length_utf8 t = Uutf.String.fold_utf_8 ... t`
  - Includes a patch file (`local-restriction.patch`) documenting the change
  </details>
- **backoff (0.1.1)**: Add runtime5 detection for Domain module compatibility with OCaml 4.x
  <details><summary>Details</summary>

  - Add `%runtime5` external to detect OCaml 5 runtime at startup
  - Create conditional `cpu_relax` function that is a no-op on OCaml 4.x
  - Create conditional `recommended_domain_count` function returning 1 on OCaml 4.x
  - Replace direct `Domain.cpu_relax` and `Domain.recommended_domain_count` calls with the conditional versions
  - Add `@@ portable` annotation to the `.mli` file
  - Include patch file documenting the changes
  </details>
- **cohttp (6.0.0~beta2)**: Local version is pinned to v6.0.0~beta2 with forward proxy support removed and API changes reverted for compatibility with older OCaml and dependency versions.
  <details><summary>Details</summary>

  - **Version pinned to 6.0.0~beta2**: Local version uses an older release (6.0.0~beta2) instead of upstream 6.2.1
  - **Proxy module removed**: The `Cohttp.Proxy` module for forward proxy support is completely removed from local version
  - **Request/Response API differences**:
    - Request type retains `scheme` and `encoding` fields that were removed in upstream
    - Response type retains `encoding` and `flush` fields
    - `make_for_client` has simpler signature (no `?absolute_form` parameter)
    - `flush` parameter changed from labeled to optional in various functions
  - **Connection cache changes**: 
    - `Make_tunnel` and `Make_proxy` modules removed from connection cache
    - Proxy-related code removed from `Connection` module
    - `create_tunnel` function removed
  - **Net module changes**:
    - `default_ctx` changed from `ctx Lazy.t` to plain `ctx`
    - `client` type and `tunnel` function removed
  - **Dependency changes**:
    - Uses `mirage-crypto-rng-eio` instead of `mirage-crypto-rng.unix`
    - Uses older versions of `alcotest`, `ounit` instead of `ounit2`
    - Removes `ipaddr` and `uri.services` dependencies from cohttp core
    - Uses `digestif.c` replaced with `mirage-crypto` in examples
  - **Body.is_empty API change**: Returns `bool Deferred.t` instead of `` [`True | `False | `Unknown] ``
  - **OCaml version support**: Tests against OCaml 4.08-4.14 instead of OCaml 5.x only
  - **Error handling**: Uses `Lwt.fail` instead of `raise` in various places for better async error propagation
  - **cohttp-eio changes**: Simplified Client/Server APIs, removed proxy support, different TLS initialization
  </details>
- **conduit (8.0.0)**: Remove test SSL configuration file for monorepo compatibility
  <details><summary>Details</summary>

  - Deleted `tests/conduit-lwt-unix/server.conf` - an OpenSSL configuration file used for generating test certificates
  - The file contained certificate request settings for localhost testing with TLS
  - Removal likely simplifies test infrastructure or avoids certificate generation dependencies in the monorepo
  </details>
- **cppo (1.8.0)**: Add opam package metadata file for monorepo integration
  <details><summary>Details</summary>

  - Added `opam` file with package metadata (version 1.8.0, BSD-3-Clause license)
  - Specifies dependencies: ocaml (>= 4.02.3), dune (>= 2.0), base-unix
  - Includes build instructions for dune with test and doc targets
  - Upstream already has dune-project, so dune support is not new
  </details>
- **ctypes**: Downgrade to version 0.23.0 with bigarray-compat for older OCaml support and dune 2.9 compatibility
  <details><summary>Details</summary>

  - **Version downgrade**: From 0.24.0 to 0.23.0, removing features that require OCaml 4.07+
  - **OCaml version support**: Lowered minimum OCaml version from 4.07.0 to 4.03.0
  - **Dune version**: Downgraded from dune 3.9 to dune 2.9
  - **Bigarray compatibility**: Replaced all `Bigarray` references with `Bigarray_compat` for older OCaml support
  - **Added dependencies**: `bigarray-compat` (runtime), `stdlib-shims` (tests)
  - **Removed upstream features**: 
    - Removed passability bypass for incomplete types in function pointers (`@->` and `returning` overrides in cstubs_structs.ml)
    - Removed `new_chorse_as_animal` test function and related funptr test types
  - **Build system changes**:
    - Changed `build_if` to `enabled_if` with tuareg dune file for fts example
    - Added `--promote-install-files=false` and separate install step for opam
    - Removed warning flags `-67-69`
  - **CI changes**: Added OCaml 4.03-4.06 to test matrix, removed 5.3.0, downgraded setup-ocaml to v2
  - **OxCaml patch**: Includes a `bigarray.patch` file suggesting an `@ immutable` mode annotation for `bigarray_kind` function
  </details>
- **dune (3.21.0)**: Eta-expand partially applied functions for OxCaml compiler compatibility
  <details><summary>Details</summary>

  - **v1.ml (configurator)**: Eta-expand `create_process` function to take explicit `stdin`, `stdout`, `stderr` arguments instead of partial application of `Unix.create_process` and `Unix.create_process_env`
  - **v1.ml (configurator)**: Wrap `prerr_endline` and `log` in lambdas (`fun s -> prerr_endline s`) instead of using them as bare function values
  - **fpath.ml (stdune)**: Eta-expand `Unix.unlink` to `fun s -> Unix.unlink s` instead of direct function reference
  - **csexp_rpc.ml**: Rename parameters from `t b` to `fd bytes pos len` and eta-expand `send` and `Unix.single_write` calls
  - **dune_trace.ml**: Wrap `Stdlib.output_string out` in a lambda (`fun s -> Stdlib.output_string out s`)
  - **oxcaml-dune.patch**: Added patch file documenting all the above changes
  - **boot/dune.install**: Removed (cleanup)
  </details>
- **gen (1.1)**: Remove seq library dependency and delete unused qtest/benchmark infrastructure
  <details><summary>Details</summary>

  - Remove `(libraries seq)` dependency from `src/dune` - the `seq` module is now part of the OCaml standard library
  - Delete `bench/.merlin` file (legacy editor support file)
  - Delete `qtest/Makefile` and `qtest/dune` (test infrastructure not needed in monorepo)
  </details>
- **gen_js_api (1.1.2)**: Compatibility fixes for OxCaml extended AST and ppxlib_jane, plus modernization patches
  <details><summary>Details</summary>

  - **ppxlib_jane integration**: Added `ppxlib_jane` dependency to handle OxCaml AST extensions
  - **Extended AST compatibility in gen_js_api_ppx.ml**:
    - Updated `Ptyp_arrow` patterns to handle additional mode/modality parameters `(lab, t1, t2, _, _)`
    - Updated `Ptyp_tuple` to use `Ppxlib_jane.as_unlabeled_tuple` for labeled tuple support
    - Updated `Ptyp_var` patterns to handle jkind parameter `(label, _)`
    - Updated `Ptyp_any` patterns to handle jkind parameter
    - Updated `Pmty_functor` patterns to handle modalities `(params, body, [])`
    - Updated `Psig_include` to handle modalities tuple `(info, moda)`
    - Updated `Pcstr_tuple` args to use `Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type`
    - Updated module declarations to use `Ppxlib_jane.Shim.Module_declaration.of_parsetree`
    - Changed signature parsing to handle `psg_items` field
  - **AST builder changes**: Replaced `Exp.fun_` with `Ast_builder.Default.pexp_fun` throughout
  - **Signature type fix**: Changed `Mty.signature` to `Mty.mk (Pmty_signature ...)`
  - **Parse interface fix**: Updated `Pparse.parse_interface` call to access `.ast` field
  - **Runtime fixes**:
    - Changed `joo_global_object` to `globalThis` in ojs.ml for modern JavaScript compatibility
    - Added `[@ocaml.alert "-unsafe_multidomain"]` to suppress OCaml 5 multicore warning in ojs_exn.ml
  - **PPX driver fix**: Disabled ppxlib location check to allow `[@@deriving sexp]` with `[@@js]` attributes
  - **Patch files**: Includes `.patch` files documenting all changes for reproducibility
  </details>
- **hex (1.5.0)**: Remove obsolete .merlin file for cleaner monorepo integration
  <details><summary>Details</summary>

  - Removed `.merlin` file (obsolete editor configuration from pre-dune era)
  - No code changes; all source files remain identical to upstream
  </details>
- **jingoo**: Remove .gitignore file for monorepo compatibility
  <details><summary>Details</summary>

  - Removed `.gitignore` file which contained package-specific ignore patterns that are not needed in the monorepo context
  </details>
- **lwt (6.0.0)**: Compatibility fixes for OxCaml compiler's modified AST representation and type system
  <details><summary>Details</summary>

  - **PPX AST compatibility fixes in `ppx_lwt.ml`**:
    - Updated `Ppat_constraint` pattern to handle extra parameter: `(p,_)` → `(p,_,_)`
    - Updated `Pexp_let` pattern for extra parameter: `(Nonrecursive, ...)` → `(_, Nonrecursive, ...)`
    - Changed `pexp_function_cases` to `pexp_function` (5 occurrences) to match modified ppxlib API
  - **Unix socket functions in `lwt_unix.cppo.ml`**:
    - Changed 4 socket operations (`recv`, `send`, `recvfrom`, `sendto`) from value-level conditionals to inline function definitions to work around type system differences
  - Includes `oxcaml.patch` file documenting all the changes
  </details>
- **menhir (20250912)**: Monorepo compatibility changes: add opam file, remove documentation and editor files
  <details><summary>Details</summary>

  - Added `opam` file with package metadata for monorepo integration (version 20250912, dependencies on menhirLib, menhirSdk, menhirCST)
  - Removed `doc/manual.html` (large generated HTML documentation not needed in monorepo)
  - Removed `src/.merlin` (editor helper file with build paths, not needed in monorepo)
  </details>
- **psq (0.2.1)**: Remove seq library dependency for monorepo compatibility
  <details><summary>Details</summary>

  - Remove `(libraries seq)` from `src/dune` - the `seq` module is part of the OCaml standard library since OCaml 4.07 and doesn't need to be declared as an explicit dependency in the monorepo environment
  </details>
- **sedlex (3.6)**: Adapt ppx_sedlex for OxCaml/ppxlib_jane AST changes with labeled tuples and ghost location handling
  <details><summary>Details</summary>

  - **Labeled tuple support**: Updated `Ppat_tuple` pattern matching to handle the new OxCaml AST format with labeled tuples (`([None, p0; None, p1], Closed)`) using `Ppxlib_jane.as_unlabeled_tuple`
  - **Function construction**: Replaced `Ast_helper.Exp.fun_` with `Ppxlib_jane.Ast_builder.Default.add_fun_param` for OxCaml compatibility
  - **Ghost location handling**: Added `loc_ghoster` AST traverser to mark generated code locations as ghost, preventing spurious source location issues in generated code
  - **Removed `.merlin` file**: Deleted obsolete Merlin configuration file
  - **Included patch file**: Added `sedlex+syntax+ppx_sedlex.ml.patch` documenting the changes
  </details>
- **topkg (1.0.8)**: Remove redundant function declarations from interface to fix OCaml compatibility
  <details><summary>Details</summary>

  - Removed `val trim : string -> string` declaration from `topkg_string.mli`
  - Removed `val uppercase_ascii : string -> string` declaration from `topkg_string.mli`
  - Both functions are still available via `include module type of String`, so this removes redundant re-declarations
  - Includes a patch file documenting the interface changes
  - This appears to fix compatibility issues where these re-declarations may conflict with the standard library's String module in newer OCaml versions
  </details>
- **tyxml (4.6.0)**: Remove seq library dependency since it's now part of OCaml stdlib
  <details><summary>Details</summary>

  - Remove `seq` from library dependencies in `lib/dune` since the `Seq` module is part of the OCaml standard library in OCaml 4.07+
  </details>

## Build Fixes

Packages with build system fixes

- **afl-persistent**: Fix config.sh to return to original directory instead of root after cleanup
  <details><summary>Details</summary>

  - Modified `config.sh` to save the current directory before creating a temp directory
  - Changed `cd /` to `cd "$curdir"` after cleanup, returning to the original working directory instead of the root filesystem
  - Added `config.patch` file containing the same fix as a patch for reference
  </details>
- **binaryen (119)**: Different codebases: pristine is grain-lang/binaryen.ml OCaml bindings, local is upstream WebAssembly/binaryen C++ tools with opam packaging
  <details><summary>Details</summary>

  - **Pristine (./sources/binaryen)**: OCaml bindings for Binaryen from grain-lang/binaryen.ml
    - Uses dune build system with OCaml FFI stubs
    - Package name: `binaryen` (OCaml library)
    - Depends on `libbinaryen` C library
  - **Local (./opam/binaryen)**: Full upstream WebAssembly/binaryen C++ toolchain
    - Package name in opam: `binaryen-bin` (provides wasm-opt, wasm-metadce, wasm-merge binaries)
    - Uses cmake/ninja build system via opam
    - Contains full C++ source (~35MB diff due to entirely different codebase)
  - These appear to be **completely different packages** with similar names that serve complementary purposes
  </details>
- **ocamlbuild**: Fix for-pack tag propagation to .ml/.mli files and add directory-specific build rules
  <details><summary>Details</summary>

  - **Makefile changes**: Replace generic `%.cmo` and `%.cmi` rules with directory-specific rules for `src/`, `bin/`, and `plugin-lib/` directories. The `src/` directory rules include `-for-pack Ocamlbuild_pack` flag
  - **configuration.ml changes**: Add logic to propagate `for-pack(...)` tags from `.cmx` files to corresponding `.ml` and `.mli` files, ensuring pack-related tags are applied during bytecode compilation
  - **flambda2.patch**: Included patch file documenting these changes (appears to be for flambda2 compatibility)
  - **.depend file removed**: The dependency file was removed from the modified version
  </details>

## JaneStreet

Packages from github.com/janestreet or github.com/oxcaml (unmodified)

abstract_algebra, accessor, accessor_async, accessor_base, accessor_core, am_running_how_js, async, async_durable, async_extra, async_find, async_inotify, async_interactive, async_iterator, async_js, async_kernel, async_log, async_rpc_kernel, async_rpc_websocket, async_sendfile, async_shell, async_smtp, async_ssl, async_udp, async_unix, async_websocket, await, babel, base, base_bigstring, base_quickcheck, base_trie, basement, bidirectional_map, big_percent, bigdecimal, bignum, bin_prot, bitset, bonsai, bonsai_bench, bonsai_concrete, bonsai_examples, bonsai_term, bonsai_term_components, bonsai_term_examples, bonsai_term_test, bonsai_test, bonsai_web, bonsai_web_components, bonsai_web_test, capitalization, capsule, capsule0, codicons, cohttp_async_websocket, cohttp_static_handler, command_nodejs, command_rpc, concurrent, content_security_policy, core, core_bench, core_extended, core_kernel, core_profiler, core_unix, csvfields, curl_async, current_exe, dedent, delimited_parsing, ecaml, email_message, env_config, expect_test_helpers_async, expect_test_helpers_core, expectable, expectree, fieldslib, file_path, filesystem, flexible_sexp, float_array, font_awesome_icons, fuzzy_match, fzf, handled_effect, hardcaml, hardcaml_axi, hardcaml_c, hardcaml_circuits, hardcaml_event_driven_sim, hardcaml_fixed_point, hardcaml_handshake, hardcaml_hobby_boards_demos, hardcaml_hobby_boards_kernel, hardcaml_of_verilog, hardcaml_step_testbench, hardcaml_template_project, hardcaml_test_harness, hardcaml_verify, hardcaml_verilator, hardcaml_waveterm, hardcaml_xilinx, hardcaml_xilinx_components, hardcaml_xilinx_reports, heterogeneous_list, hex_encode, hg_lib, higher_kinded, incr_dom, incr_dom_interactive, incr_dom_partial_render, incr_dom_sexp_form, incr_map, incr_select, incremental, indentation_buffer, insertion_ordered_map, int_repr, jane-street-headers, jane_rope, janestreet_cpuid, janestreet_csv, janestreet_lru_cache, janestreet_shims, js_of_ocaml_patches, jsonaf, jst-config, legacy_diffable, line-up-words, lsp_rpc, man_in_the_middle_debugger, maybe_pushback, mdx, memtrace, memtrace_viewer, merlin (5.2.1-502+ox), mlt_parser, n_ary, netsnmp, nonempty_interval_lib, notty_async, numeric_string, ocaml-compiler-libs (v0.17.0), ocaml-embed-file, ocaml-probes, ocaml_intrinsics, ocaml_intrinsics_kernel, ocaml_openapi_generator, ocaml_simd, ocamlformat, of_json, oklab, ordinal_abbreviation, oxcaml_intrinsics, pam, parallel, parsexp, parsexp_io, parsexp_prefix, parsexp_symbolic_automaton, patdiff, patience_diff, pending_or_error, pipe_with_writer_error, polling_state_rpc, portable, portable_lockfree_htbl, portable_ws_deque, posixat, postgres_async, ppx_accessor, ppx_anonymous_record, ppx_array, ppx_array_base, ppx_assert, ppx_base, ppx_bench, ppx_bin_prot, ppx_box, ppx_cold, ppx_compare, ppx_conv_func, ppx_css, ppx_csv_conv, ppx_custom_printf, ppx_debug_assert, ppx_demo, ppx_derive_at_runtime, ppx_diff, ppx_disable_unused_warnings, ppx_embed_file, ppx_enumerate, ppx_expect, ppx_fields_conv, ppx_fixed_literal, ppx_for_loop, ppx_fuelproof, ppx_globalize, ppx_hardcaml, ppx_hash, ppx_helpers, ppx_here, ppx_html, ppx_ignore_instrumentation, ppx_inline_test, ppx_int63_literal, ppx_jane, ppx_js_style, ppx_jsonaf_conv, ppx_let, ppx_log, ppx_module_timer, ppx_optcomp, ppx_optional, ppx_pattern_bind, ppx_pipebang, ppx_portable, ppx_quick_test, ppx_rope, ppx_sexp_conv, ppx_sexp_message, ppx_sexp_value, ppx_shorthand, ppx_simple_xml_conv, ppx_stable, ppx_stable_witness, ppx_string, ppx_string_conv, ppx_template, ppx_tydi, ppx_typed_fields, ppx_typerep_conv, ppx_var_name, ppx_variants_conv, ppx_with, ppx_xml_conv, ppx_yojson_conv, ppx_yojson_conv_lib, ppxlib_jane, profunctor, protocol_version_header, re2, re_parser, record_builder, redis-async, regex_parser_intf, resource_cache, result (1.5), rpc_parallel, semantic_version, sequencer_table, sexp, sexp_diff, sexp_grammar, sexp_macro, sexp_pretty, sexp_select, sexp_string_quickcheck, sexp_type, sexplib, sexplib0, shell, shexp, simple_xml, spawn, splay_tree, splittable_random, stdio, stored_reversed, streamable, string_dict, testable_timeout, textutils, textutils_kernel, tilde_f, time_ago, time_now, timezone, toplayer, toplevel_backend, toplevel_expect_test, topological_sort, torch, tracing, typerep, unboxed, unboxed_datatypes, unique, univ_map, unsafe_clear, uopt, uri_parsing, username_kernel, utop, variantslib, vcaml, vec, versioned_polling_state_rpc, virtual_dom, xpath, zarith_stubs_js, zstandard

## Unchanged

Packages with no modifications from upstream

angstrom (0.16.1), asn1-combinators (0.3.2), base64 (3.5.2), bigarray-compat (1.1.0), bigstringaf (0.10.0), ca-certs (1.0.1), camlp-streams, checkseum (0.5.2), cmdliner (1.3.0), crowbar, crunch (4.0.0), cryptokit, csexp (1.5.2), cstruct (6.2.0), decompress (1.5.3), digestif (1.3.0), domain-local-await (1.0.1), domain-name (0.5.0), dune-compiledb (0.6.0), either (1.0.0), eqaf (0.10), ezjsonm (1.3.0), faraday (0.8.2), fiber (3.7.0), fix, gmap (0.3.0), httpaf (0.7.1), inotify (2.6), integers, iomux (0.4), ipaddr (5.6.1), jsonfeed (1.1.0), kdf (1.0.0), lambda-term (3.2.0), lambdasoup, lwt-dllist (1.1.0), magic-mime (1.3.1), markup, mew, mew_vi, mirage-crypto (2.0.2), num (1.7~dev), ocaml-syntax-shims (1.0.0), ocaml-version, ocamlgraph (2.0.0), ocp-indent (1.9.0), ocplib-endian, ocurl (0.10.0), ohex, opam, opam-file-format (2.2.0), optint (0.3.0), owee (0.8), patch (3.1.0), pcre (8.0.5), pp (2.0.0), ppx_blob (0.9.0), ppx_derivers, progress (0.5.0), sha (1.15.4), sitemap (1.0), stdlib-shims (0.3.0), stringext (1.6.0), swhid_core, syndic (1.7.0), thread-table (1.0.0), tls (2.0.3), trie, uchar, uri (4.4.0), uring (2.7.0), x509 (1.0.6), yojson (2.2.2), zed

