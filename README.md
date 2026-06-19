# WACC Compiler Studio / WACC 编译器工作台

WACC Compiler Studio is a WACC compiler with a browser-based editor. It compiles WACC programs to AArch64 or ARM32 assembly, can apply peephole optimisation, and can run the generated program through the matching GNU cross-toolchain and QEMU emulator.

WACC Compiler Studio 是一个带浏览器编辑器的 WACC 编译器。它可以将 WACC 程序编译为 AArch64 或 ARM32 汇编代码，可启用窥孔优化，并通过对应的 GNU 交叉工具链和 QEMU 模拟器运行生成的程序。

The browser is only the client: compilation, linking, and execution all happen on the server. A modern browser is therefore sufficient on Windows, macOS, Linux, phones, and tablets.

浏览器只负责客户端界面；编译、链接和执行都在服务器上完成。因此 Windows、macOS、Linux、手机和平板设备只需使用现代浏览器即可访问。

## Features / 功能

- Compile WACC source to **AArch64** or **ARM32** assembly. / 将 WACC 源代码编译为 **AArch64** 或 **ARM32** 汇编。
- Enable or disable peephole optimisation. / 启用或关闭窥孔优化。
- Run generated assembly with a GNU cross-compiler and QEMU. / 使用 GNU 交叉编译器和 QEMU 运行生成的汇编程序。
- Provide ready-to-use examples, health endpoints, bounded job queues, rate limiting, and security headers. / 提供示例程序、健康检查端点、受限任务队列、限流和安全响应头。
- Deploy as a Docker service on Render. / 支持以 Docker 服务部署到 Render。

## Project layout / 项目结构

| Path / 路径 | Purpose / 作用 |
| --- | --- |
| `src/main/wacc` | Scala compiler: frontend, intermediate representation, and AArch64/ARM32 backends. / Scala 编译器：前端、中间表示和 AArch64/ARM32 后端。 |
| `src/test/wacc` | Scala unit and integration tests. / Scala 单元测试与集成测试。 |
| `examples` | Valid and invalid WACC example programs. / 合法和非法的 WACC 示例程序。 |
| `web` | Node.js HTTP server, browser editor, and web-server tests. / Node.js HTTP 服务、浏览器编辑器和 Web 服务测试。 |
| `Dockerfile`, `docker-compose.yml` | Reproducible full runtime with the compiler, cross-toolchains, and QEMU. / 包含编译器、交叉工具链和 QEMU 的可复现完整运行环境。 |
| `render.yaml` | Render Blueprint configuration. / Render Blueprint 配置。 |

## Local development / 本地开发

### Requirements / 前置条件

- Node.js 18 or later / Node.js 18 或更高版本
- Either a built `./wacc-compiler` executable or Scala CLI/Scala available on `PATH` / 已构建的 `./wacc-compiler` 可执行文件，或 `PATH` 中可用的 Scala CLI/Scala

Start the browser application:

启动浏览器应用：

```bash
npm start
```

Then open [http://127.0.0.1:3000](http://127.0.0.1:3000). The server first uses `WACC_COMPILER` when set, then `./wacc-compiler`, and otherwise falls back to `scala run`.

然后打开 [http://127.0.0.1:3000](http://127.0.0.1:3000)。服务会优先使用 `WACC_COMPILER` 指定的程序，其次使用 `./wacc-compiler`，否则回退到 `scala run`。

To build the native compiler used by the web server:

构建 Web 服务使用的本地编译器：

```bash
make
```

Run the web-server tests:

运行 Web 服务测试：

```bash
npm run test:web
```

Run the Scala compiler test suite:

运行 Scala 编译器测试套件：

```bash
scala-cli test . --server=false --jvm system
```

## Running generated programs / 运行生成的程序

Compiling assembly works without an ARM runtime. Running generated code requires the following cross-toolchain and emulator for the selected target.

仅生成汇编代码时不需要 ARM 运行环境。运行生成的代码则需要目标架构对应的交叉工具链和模拟器。

| Target / 目标 | Assembler and linker / 汇编器与链接器 | Emulator / 模拟器 | Default sysroot / 默认 sysroot |
| --- | --- | --- | --- |
| AArch64 | `aarch64-linux-gnu-gcc` | `qemu-aarch64` | `/usr/aarch64-linux-gnu/` |
| ARM32 | `arm-linux-gnueabi-gcc` | `qemu-arm` | `/usr/arm-linux-gnueabi/` |

On Debian or Ubuntu, install them with:

在 Debian 或 Ubuntu 上，可使用以下命令安装：

```bash
sudo apt install gcc-aarch64-linux-gnu gcc-arm-linux-gnueabi qemu-user
```

Override tool locations with `WACC_AARCH64_GCC`, `WACC_AARCH64_QEMU`, `WACC_AARCH64_SYSROOT`, `WACC_ARM32_GCC`, `WACC_ARM32_QEMU`, and `WACC_ARM32_SYSROOT`.

可以通过 `WACC_AARCH64_GCC`、`WACC_AARCH64_QEMU`、`WACC_AARCH64_SYSROOT`、`WACC_ARM32_GCC`、`WACC_ARM32_QEMU` 和 `WACC_ARM32_SYSROOT` 覆盖工具路径。

## Docker / Docker 容器

Docker provides the supported complete runtime. The image builds the Scala compiler and includes both GNU cross-toolchains, Linux sysroots, and QEMU.

Docker 提供受支持的完整运行环境。镜像会构建 Scala 编译器，并包含两个 GNU 交叉工具链、Linux sysroot 和 QEMU。

```bash
docker compose up --build
```

Open [http://127.0.0.1:3000](http://127.0.0.1:3000) on the host. Other devices on the same network can use `http://<computer-lan-ip>:3000`; allow inbound TCP port 3000 in the host firewall.

在宿主机上打开 [http://127.0.0.1:3000](http://127.0.0.1:3000)。同一局域网中的其他设备可访问 `http://<computer-lan-ip>:3000`；请在宿主机防火墙中允许入站 TCP 3000 端口。

The container runs without root privileges or Linux capabilities, uses a read-only filesystem, limits process count, and has a temporary 256 MB compilation directory.

容器以非 root 用户运行且不具备 Linux capabilities，使用只读文件系统，限制进程数量，并提供 256 MB 的临时编译目录。

## Deploy on Render / 部署到 Render

`render.yaml` defines a Docker-based HTTPS web service. To deploy it:

`render.yaml` 定义了一个基于 Docker 的 HTTPS Web 服务。部署步骤如下：

1. Push this repository to GitHub or GitLab. / 将此仓库推送到 GitHub 或 GitLab。
2. In Render, create a new Blueprint and connect the repository. / 在 Render 中创建 Blueprint 并连接仓库。
3. Apply the detected `render.yaml` service. / 应用识别到的 `render.yaml` 服务。
4. Open the assigned `https://...onrender.com` URL from any device. / 在任意设备上打开分配的 `https://...onrender.com` 地址。

Client devices do not need a compiler, GCC, QEMU, Docker, or WACC files. Each submitted program is processed in its own temporary directory and removed after the request completes.

客户端设备不需要安装编译器、GCC、QEMU、Docker 或 WACC 文件。每个提交的程序都在独立的临时目录中处理，并会在请求完成后删除。

Useful production settings / 常用生产环境配置：

| Variable / 变量 | Default / 默认值 | Purpose / 用途 |
| --- | ---: | --- |
| `MAX_CONCURRENT_JOBS` | `2` | Simultaneous compiler jobs / 同时执行的编译任务数 |
| `MAX_QUEUED_JOBS` | `20` | Requests waiting for a worker / 等待工作进程的请求数 |
| `RATE_LIMIT_REQUESTS` | `30` | Compile requests per client per window / 每个客户端在窗口期内的编译请求数 |
| `RATE_LIMIT_WINDOW_MS` | `60000` | Rate-limit window in milliseconds / 限流窗口（毫秒） |
| `TRUST_PROXY` | `0` | Set to `1` behind Render or another trusted proxy / 位于 Render 或其他可信代理后时设为 `1` |

## Contributing / 参与开发

Keep compiler changes and web-service changes covered by the relevant tests. Do not commit generated binaries, build output, or temporary files. Use a focused commit message that describes the user-visible change or the compiler component affected.

修改编译器或 Web 服务时，请运行对应测试。不要提交生成的二进制文件、构建产物或临时文件。提交信息应聚焦描述用户可见的变化，或受影响的编译器组件。
