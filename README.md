# Mining Client for Kadena

A mining client for Kadena's chainweb node mining API. It supports

* mining with an ASIC via Kadena Stratum (JSON RPC over TCP)
* simulated mining for testing.
* multi threaded CPU mining,
* external mining workers (e.g. a GPU),

*Competitive mining on the Kadena Mainnet requires special mining hardware
(ASIC), which connect to a Stratum Server from where they obtain work.*

*All other mining modes (GPU, CPU, and simulation) intended only for testing.*

*   [Installation](#installation)
*   [Usage](#usage)
*   [Usage Examples](#usage-examples)
    *  [Generating a New Key Pair](#generating-a-new-key-pair)
    *  [Mining on Mainnet With an ASIC](#mining-on-mainnet-with-an-asic)
    *  [CPU Mining](#cpu-mining)
    *  [GPU Mining](#gpu-mining)
    *  [Creating a Configuration File](#creating-a-configuration-file)

## Installation

A docker image of the latest version is available at
`ghcr.io/kadena-io/chainweb-mining-client:lastest`.

Prebuild binaries for the latest version are available from the [GitHub Release
Site](https://github.com/kadena-io/chainweb-mining-client/releases/latest).

Binaries can also be compiled with a recent version of GHC and cabal directly
from [Hackage](https://hackage.haskell.org/package/chainweb-mining-client)

```sh
cabal install chainweb-mining-client
```

or from the GitHub sources


```sh
git clone https://github.com/kadena-io/chainweb-mining-client/
cd chainweb-mining-client
cabal build
cabal run chainweb-mining-client -- --help
```

## Usage

Calling `chainweb-mining-client --help` provides an overview of the available
usage options:

```txt
Usage: chainweb-mining-client [--info] [--long-info] [-v|--version] [--license]
                              [-?|-h|--help]
                              [--print-config-as full|minimal|diff |
                                --print-config] [--config-file FILE]
                              [-r|--hash-rate ARG] [-n|--node DOMAIN:PORT]
                              [(-t|--tls) | --no-tls]
                              [(-x|--insecure) | --no-insecure]
                              [-k|--public-key ARG] [-c|--thread-count ARG]
                              [--generate-key | --no-generate-key]
                              [-l|--log-level error|warn|info|debug]
                              [-w|--worker cpu|external|simulation|stratum]
                              [--external-worker-cmd ARG] [--stratum-port ARG]
                              [--stratum-interface ARG]
                              [--stratum-difficulty ARG]
  Kadena Chainweb Mining Client

Available options:
  --info                   Print program info message and exit
  --long-info              Print detailed program info message and exit
  -v,--version             Print version string and exit
  --license                Print license of the program and exit
  -?,-h,--help             Show this help message
  --print-config-as full|minimal|diff
                           Print the parsed configuration to standard out and
                           exit
  --print-config           Print the parsed configuration to standard out and
                           exit. This is an alias for --print-config-as=full
  --config-file FILE       Configuration file in YAML or JSON format. If more
                           than a single config file option is present files are
                           loaded in the order in which they appear on the
                           command line.
  -r,--hash-rate ARG       hashes per second (only relevant for mining
                           simulation, ignored by the cpu worker)
  -n,--node DOMAIN:PORT    node to which to connect
  -t,--tls                 use TLS to connect to node
  --no-tls                 unset flag tls
  -x,--insecure            accept self-signed TLS certificates
  --no-insecure            unset flag insecure
  -k,--public-key ARG      the public-key for the mining rewards account
  -c,--thread-count ARG    number of concurrent mining threads
  --generate-key           Generate a new key pair and exit
  --no-generate-key        unset flag generate-key
  -l,--log-level error|warn|info|debug
                           Level at which log messages are written to the
                           console
  -w,--worker cpu|external|simulation|stratum
                           The type of mining worker that is used
  --external-worker-cmd ARG
                           command that is used to call an external worker. When
                           the command is called the target value is added as
                           last parameter to the command line.
  --stratum-port ARG       the port on which the stratum server listens
  --stratum-interface ARG  network interface that the stratum server binds to
  --stratum-difficulty ARG How the difficulty for stratum mining shares is
                           choosen. Possible values are "block" for using the
                           block target of the most most recent notification of
                           new work, or number between 0 and 256 for specifiying
                           a fixed difficulty as logarithm of base 2 (number of
                           leading zeros).

Configurations are loaded in order from the following sources:
  1. Configuration files from locations provided through --config-file options
     in the order as they appear.
  2. Command line options.

Configuration file locations can be either local file system paths or remote
HTTP or HTTPS URLs. Remote URLs must start with either "http://" or "https://".

Configuration settings that are loaded later overwrite settings that were loaded
before.
```

## Usage Examples

### Generating a New Key Pair

```sh
chainweb-mining-client --generate-key
```

This outputs a public and a private key. The public key is given to the mining
client. The private key must be kept secret.

```txt
public:  87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf
private: 64ef6379db5ef6004aff98182688c6e8b4a5229e706f1ccf6a73b05b1432aedf
```

### Mining on Mainnet With an ASIC

chainweb-mining-client needs access to the mining API of a full Chainweb node in
the Kadena Mainnet. The node must be configured to enable the mining API with
the Pact public key and account name of the miner. Rewards for mined blocks will
be credited to that account.

Assuming that `example.com` serves the chainweb-node mining API on port 443, the following command can be used
to run chainweb-mining-client with the stratum server on port 1917:

```
chainweb-mining-client \
    --tls \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:443 \
    --log-level info \
    --thread-count 2 \
    --worker stratum \
    --stratum-port 1917
```

Assuming that the host on which chainweb-mining-client runs is 192.168.1.2, the
pool address that is used to configure the ASIC miner is
`stratum+tcp://192.168.1.2:1917`.

One can point more than a single ASIC miner to the same chainweb-mining-client
stratum server. All connected clients work together and all mining rewards are
credited to the same account, that is configured in the Chainweb node.

By default the stratum server sets the difficulty of the work that is sent to
the mining clients to the actual difficulty of block of the most recent work.
This means that it can take a long time before the client solves any work. It
also means that the target is reset for each new work item that is sent.

It is also possible to set a custom difficulty. As a consequence not all
accepted solutions qualify as solved blocks, but ASIC miner may provide a more
continuous feedback on its current performance. Some devices may also be more
efficient in this mode an yield higher returns.

The following command runs a miner at a fixed difficulty level:

```
chainweb-mining-client \
    --tls \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:443 \
    --worker stratum \
    --log-level info \
    --thread-count 2 \
    --stratum-port 1917 \
    --statum-difficulty=46
```

The `--stratum-difficulty` parameter expects a integral number between 0 and
256. It denotes the difficulty as a logarithm of base 2. In practice the actual
target uses a difficulty level of at least 42 and at most the difficulty of
block of the current work.

It may help to experiment a little with the `--stratum-difficulty` and the
`--thread-count` parameters. We found, that small values for `--thread-count`
and moderate values for `--stratum-difficulty` (in the upper fourties / lower
fifties) yielded good results. But that may differ between different devices and
setups.

### CPU Mining

Assuming that `example.com` serves the chainweb-node mining API on port 443, the
following command can be used for CPU mining to the from the previous example:

```sh
chainweb-mining-client \
    --tls \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cfa \
    --node example.com:443 \
    --worker cpu \
    --thread-count 16 \
    --log-level info
```

If the chainweb-node uses a self-signed certificate the flag `--insecure` can be
used to disable certificate validation.

If a version of chainweb-node is used that serves the mining API on a separate
port using plain HTTP, the `--tls` flag is ommitted.

### GPU Mining

GPU mining is supported via calling an external worker that does the mining
work. An example for such a GPU mining tool for Kadena is
[bigolchunugus](https://github.com/kadena-community/bigolchungus).

```sh
chainweb-mining-client \
    --tls \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:443 \
    --worker external \
    --external-miner-cmd bigolchunus
```

For selecting the GPU or passing additional arguments to the external tools, the
extra parameters are appended to the command. E.g.

```sh
chainweb-mining-client \
    --tls \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:443 \
    --worker external \
    --external-worker-cmd "bigolchunus -d 2"
```

The external mining tool is called by adding the target as the last command line
parameter and passing the work bytes on stdin.

### Creating a Configuration File

The current configuration of chainweb-mining-client can be stored in a
configuration file by adding the `--print-config` parameter

```sh
chainweb-mining-client \
    --tls \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:443 \
    --worker external \
    --external-worker-cmd "bigolchunus -d 2" \
    --print-config > config.yml
```

This would result in the following configuration file:

```yaml
logLevel: info
threadCount: 10
node: example.com:443
publicKey: 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf
externalWorkerCommand: bigolchunus -d 2
generateKey: false
hashRate: 1000000.0
useTls: true
insecure: true
worker: external
```

The configuration can then be used via

```sh
chainweb-mining-client --config-file config.yml
```
