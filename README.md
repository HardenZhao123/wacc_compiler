# WACC Compiler Studio

This repository contains a Scala implementation of a WACC compiler and a browser-based compiler studio. The compiler parses, renames, type-checks, lowers to TAC, emits ARM32 or AArch64 assembly, and optionally applies peephole optimisation. The web app lets you edit WACC programs, choose a target architecture, inspect generated assembly, and run the program through the configured cross toolchain and QEMU.

The WACC language reference is maintained separately from this README. Start the web app and click **Language spec** to read the full syntax and semantic rules. The source for that reference lives at `web/public/wacc-language-spec.md`.

## Repository Layout

```text
src/main/wacc/frontend        Lexer, parser, AST, renamer, return checker, type checker
src/main/wacc/backend         TAC lowering plus ARM32 and AArch64 code generation
src/test/wacc                 Unit and integration tests
examples/valid                Accepted WACC programs grouped by language feature
examples/invalid              Syntax and semantic rejection examples
web/public                    Browser UI assets
web/server.js                 Local HTTP server and compile/run API
Dockerfile                    Container build for the full web compiler studio
```

## Requirements

- Scala CLI, or a `scala` command that provides Scala CLI-compatible project commands.
- Node.js 18 or newer for the web server and web tests.
- Optional for native packaged builds: GraalVM native-image support.
- Optional for running generated assembly: ARM cross compilers and QEMU.

The compiler can still generate assembly without the execution toolchains. Running programs from the web app requires the target linker, emulator, and sysroot.

## Command Line Usage

Compile a program directly through Scala CLI:

```sh
scala-cli run . --server=false --jvm system -- examples/valid/IO/print/println.wacc --architecture aarch64 --peephole-optim
```

Use the generated wrapper after building `wacc-compiler`:

```sh
./compile examples/valid/IO/print/println.wacc --architecture arm32 --no-peephole
```

CLI options:

```text
compile <file.wacc> [--architecture aarch64|arm32] [--peephole-optim] [--no-peephole]
```

- `--architecture aarch64|arm32`: selects the assembly backend. The CLI defaults to ARM32 if omitted.
- `--peephole-optim`: enables peephole optimisation. This is the default.
- `--no-peephole`: disables peephole optimisation.

The compiler writes a `.s` assembly file next to the source program.

Compiler exit codes:

```text
0     success
1     command-line usage error
100   syntax error
200   semantic error
255   runtime error emitted by generated programs
```

## Building

For day-to-day development, running through Scala CLI is usually enough. To rebuild the `./compile` target used by the wrapper and by the web server when present:

```sh
scala-cli --power package . --server=false --jvm system --force -o wacc-compiler
```

The provided `Makefile` builds a native-image compiler for LabTS-style environments:

```sh
make
```

After compiler changes, rebuild `wacc-compiler` or set `WACC_COMPILER` before starting the web app. If a stale `./wacc-compiler` exists, the web server will prefer it over the Scala CLI fallback.

## Web App

Start the local compiler studio:

```sh
npm start
```

Then open:

```text
http://127.0.0.1:3000
```

The web app provides:

- A WACC editor with line numbers and file upload.
- Built-in sample programs.
- ARM32 and AArch64 target selection.
- Peephole optimisation toggle.
- Assembly, compiler log, and program output tabs.
- A **Language spec** button that opens the full WACC language reference.

Useful server environment variables:

```text
HOST                    bind host, default 127.0.0.1
PORT                    bind port, default 3000
WACC_COMPILER           compiler executable to run instead of ./wacc-compiler or Scala CLI
MAX_CONCURRENT_JOBS     compile jobs allowed at once, default 2
MAX_QUEUED_JOBS         queued compile jobs, default 20
RATE_LIMIT_REQUESTS     compile requests per rate-limit window, default 30
RATE_LIMIT_WINDOW_MS    rate-limit window, default 60000
TRUST_PROXY             set to 1 when deployed behind a trusted reverse proxy
```

## Running Generated Programs

The web server runs generated assembly by linking and executing it with target-specific toolchains:

```text
AArch64: aarch64-linux-gnu-gcc, qemu-aarch64, /usr/aarch64-linux-gnu/
ARM32:   arm-linux-gnueabi-gcc, qemu-arm, /usr/arm-linux-gnueabi/
```

Override these paths when your tools are installed elsewhere:

```text
WACC_AARCH64_GCC
WACC_AARCH64_QEMU
WACC_AARCH64_SYSROOT
WACC_ARM32_GCC
WACC_ARM32_QEMU
WACC_ARM32_SYSROOT
```

## Tests

Run the Scala test suite:

```sh
scala-cli test . --server=false --jvm system
```

Run the web server tests:

```sh
npm run test:web
```

The web tests use fake compiler and toolchain executables, so they do not require ARM toolchains.

## Docker

Build and run the deployable web compiler studio:

```sh
docker build -t wacc-compiler-studio .
docker run --rm -p 3000:3000 wacc-compiler-studio
```

The image includes the packaged compiler, Node server, cross compilers, sysroots, and QEMU.

## Development Notes

- The language specification shown in the app is `web/public/wacc-language-spec.md`.
- Add new accepted programs under `examples/valid` and rejected programs under `examples/invalid`.
- Keep generated `.s` files, local binaries, and build output out of commits unless they are intentionally part of a fixture.
- Prefer explicit `--architecture` values in scripts so ARM32/AArch64 differences are easy to see.
