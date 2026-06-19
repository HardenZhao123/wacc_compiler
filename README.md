This is the provided git repository for the WACC compilers lab. You should work
in this repository regularly committing and pushing your work back to GitLab.

# Web compiler

The repository includes a deployable web interface for writing WACC, compiling
to AArch64 or ARM32 assembly, enabling/disabling peephole optimisation, and
running the generated program. The browser is only the client: compilation,
linking, and QEMU execution happen on the server. Phones, tablets, Windows,
macOS, and Linux devices therefore need only a modern browser.

## Local development

```bash
npm start
```

Open `http://127.0.0.1:3000`. The server uses the existing `./wacc-compiler`. If
that binary is absent it falls back to `scala run`. Set
`WACC_COMPILER=/absolute/path/to/compiler` to select another compiler
executable.

Assembly compilation works without an ARM runtime. Running programs uses the
same cross-compiler and QEMU pipeline as the backend integration tests:

| Target | Assembler/linker | Emulator | Default sysroot |
| --- | --- | --- | --- |
| AArch64 | `aarch64-linux-gnu-gcc` | `qemu-aarch64` | `/usr/aarch64-linux-gnu/` |
| ARM32 | `arm-linux-gnueabi-gcc` | `qemu-arm` | `/usr/arm-linux-gnueabi/` |

On Debian/Ubuntu these are provided by:

```bash
sudo apt install gcc-aarch64-linux-gnu gcc-arm-linux-gnueabi qemu-user
```

The command and sysroot paths can be overridden with
`WACC_AARCH64_GCC`, `WACC_AARCH64_QEMU`, `WACC_AARCH64_SYSROOT`,
`WACC_ARM32_GCC`, `WACC_ARM32_QEMU`, and `WACC_ARM32_SYSROOT`.

Run the web server tests with `npm run test:web`.

## Run on every device on the same network

Docker is the supported complete runtime. The image builds the Scala compiler
and includes both GNU cross-compilers, their Linux sysroots, and QEMU:

```bash
docker compose up --build
```

On the computer running Docker, open `http://127.0.0.1:3000`. On another device
connected to the same network, open `http://<computer-lan-ip>:3000`. The host
firewall must allow inbound TCP port 3000.

The container runs as a non-root user with no Linux capabilities, a read-only
filesystem, a bounded process count, and a temporary 256 MB compilation area.

## Deploy for access from anywhere

`render.yaml` defines a complete HTTPS deployment using the repository's
Dockerfile:

1. Push this repository to GitHub or GitLab.
2. In Render, create a new Blueprint and connect that repository.
3. Apply the detected `render.yaml` service.
4. Open the assigned `https://...onrender.com` URL from any device.

No compiler, GCC, QEMU, Docker, or WACC files are required on client devices.
Submitted source is processed in a unique temporary directory and deleted after
each request. The public server also limits request size, concurrent jobs,
queued jobs, execution time, process output, and compile requests per client.

Useful production settings:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `MAX_CONCURRENT_JOBS` | `2` | Simultaneous compiler jobs |
| `MAX_QUEUED_JOBS` | `20` | Requests waiting for a worker |
| `RATE_LIMIT_REQUESTS` | `30` | Compile requests per client per window |
| `RATE_LIMIT_WINDOW_MS` | `60000` | Rate-limit window in milliseconds |
| `TRUST_PROXY` | `0` | Set to `1` behind Render or another trusted proxy |

# Provided files/directories

## src/main

The src/main directory is where you code for your compiler should go, and just
contains a stub hello world file with a simple calculator inside.

## src/test
The src/test directory is where you should put the code for your tests, which
can be ran via `scala-cli test .`. The suggested framework is `scalatest`, the dependency
for which has already been included.

## project.scala
The `project.scala` is the definition of your project's build requirements. By default,
this skeleton has added the latest stable versions of both `scalatest` and `parsley`
to the build: you should check **regularly** to see if your `parsley` needs updating
during the course of WACC!

## compile

The compile script can be edited to change the frontend interface to your WACC
compiler. You are free to change the language used in this script, but do not
change its name.

## Makefile

Your Makefile should be edited so that running 'make' in the root directory
builds your WACC compiler. Currently running 'make' will call
`scala --power package . --server=false --jvm system --graalvm-jvm-id graalvm-java21 --native-image --force -o wacc-compiler`, producing a file called
`wacc-compiler`
in the root directory of the project. If this doesn't work for whatever reason, there are a few
different alternatives you can try in the makefile. **Do not use the makefile as you're working, it's for labts/CI!**
