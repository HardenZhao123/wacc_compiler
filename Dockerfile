FROM virtuslab/scala-cli:latest AS compiler-build

WORKDIR /source
COPY project.scala ./
COPY src/main ./src/main
RUN scala-cli --power package . --server=false --graalvm-jvm-id graalvm-java21 \
    --native-image --force --main-class wacc.Main -o /wacc-compiler -- \
    --no-fallback --report-unsupported-elements-at-runtime

RUN printf 'begin\n  println "native compiler ready"\nend\n' > /tmp/compiler-smoke.wacc \
    && cd /tmp \
    && /wacc-compiler /tmp/compiler-smoke.wacc --architecture aarch64 --peephole-optim \
    && test -s /tmp/compiler-smoke.s \
    && rm -f /tmp/compiler-smoke.wacc /tmp/compiler-smoke.s

FROM node:22-bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      gcc-aarch64-linux-gnu \
      gcc-arm-linux-gnueabi \
      libc6-dev-arm64-cross \
      libc6-dev-armel-cross \
      qemu-user \
    && rm -rf /var/lib/apt/lists/*

RUN printf 'int main(void) { return 0; }\n' > /tmp/toolchain-smoke.c \
    && aarch64-linux-gnu-gcc /tmp/toolchain-smoke.c -o /tmp/toolchain-aarch64 \
    && qemu-aarch64 -L /usr/aarch64-linux-gnu /tmp/toolchain-aarch64 \
    && arm-linux-gnueabi-gcc /tmp/toolchain-smoke.c -o /tmp/toolchain-arm32 \
    && qemu-arm -L /usr/arm-linux-gnueabi /tmp/toolchain-arm32 \
    && rm -f /tmp/toolchain-smoke.c /tmp/toolchain-aarch64 /tmp/toolchain-arm32

WORKDIR /app
COPY --from=compiler-build /wacc-compiler ./wacc-compiler
COPY package.json ./
COPY web ./web
COPY examples ./examples

ENV HOST=0.0.0.0
ENV PORT=3000
ENV WACC_COMPILER=/app/wacc-compiler
ENV MAX_CONCURRENT_JOBS=2
ENV MAX_QUEUED_JOBS=20
ENV RATE_LIMIT_REQUESTS=30

EXPOSE 3000
USER node
HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 \
  CMD node -e "fetch('http://127.0.0.1:'+(process.env.PORT||3000)+'/api/ready').then(r=>process.exit(r.ok?0:1)).catch(()=>process.exit(1))"
CMD ["npm", "start"]
