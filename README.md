# Mining Client for Kadena

A mining client for Kadena's chainweb node mining API. It supports

* mining with ASICs through a stratum server,
* simulated mining for testing,
* multi threaded CPU mining,
* external mining workers (e.g. a GPU),
* timed miners for non-PoW usecases.

*Competitive mining on the Kadena Mainnet requires special mining hardware
(ASIC), which connects to a Stratum Server from where it obtains work.*

*All other mining modes (GPU, CPU, and simulation) are intended only for testing.*

*   [Installation](#installation)
*   [Usage](#usage)
*   [Usage Examples](#usage-examples)
    *  [Generating a New Key Pair](#generating-a-new-key-pair)
    *  [Mining on Mainnet With an ASIC](#mining-on-mainnet-with-an-asic)
    *  [CPU Mining](#cpu-mining)
    *  [GPU Mining](#gpu-mining)
    *  [Creating a Configuration File](#creating-a-configuration-file)
*   [Related Resources](#related-resources)

## Installation

A docker image of the latest version is available at
`ghcr.io/kadena-io/chainweb-mining-client:latest`.

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
                              [-k|--public-key ARG] [-a|--account ARG]
                              [-c|--thread-count ARG]
                              [--generate-key | --no-generate-key]
                              [-l|--log-level error|warn|info|debug]
                              [-w|--worker cpu|external|simulation|stratum|constant-delay|on-demand]
                              [--external-worker-cmd ARG] [--stratum-port ARG]
                              [--stratum-interface ARG]
                              [--stratum-difficulty ARG] [-s|--stratum-rate ARG]
                              [--constant-delay-block-time ARG]
                              [--on-demand-interface ARG] [--on-demand-port ARG]

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
  -k,--public-key ARG      public-key for the mining rewards account
  -a,--account ARG         account for the mining rewards (default: public-key
                           prefixed with 'k:')
  -c,--thread-count ARG    number of concurrent mining threads
  --generate-key           Generate a new key pair and exit
  --no-generate-key        unset flag generate-key
  -l,--log-level error|warn|info|debug
                           Level at which log messages are written to the
                           console
  -w,--worker cpu|external|simulation|stratum|constant-delay|on-demand
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
  -s,--stratum-rate ARG    Rate (in milliseconds) at which a stratum worker
                           thread emits jobs.
  --constant-delay-block-time ARG
                           time at which a constant-delay worker emits blocks
  --on-demand-interface ARG
                           network interface that the on-demand mining server
                           binds to
  --on-demand-port ARG     port on which the on-demand mining server listens

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

chainweb-mining-client needs access to the mining API of a full [Chainweb
node](https://github.com/kadena-io/chainweb-node) in
the Kadena Mainnet. The node must be configured to enable the mining API with
the Pact *public* key (and, optionally, account name) of the miner. Rewards for
mined blocks will be credited to that account. The default is to use the `k:`
account for the key.

The `--enable-mining-coordination`, `--mining-public-key` can be used to
configure chainweb-node for mining. The mining API is served on the service API
port (default is 1848).


Assuming that `example.com` serves the chainweb-node mining API on port 1848,
the following command can be used to run chainweb-mining-client with the stratum
server on port 1917:

```
chainweb-mining-client \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:1848 \
    --stratum-port 1917
```

Assuming that the host on which chainweb-mining-client runs is 192.168.1.2, the
pool address that is used to configure the ASIC miner is
`stratum+tcp://192.168.1.2:1917`.

One can point more than a single ASIC miner to the same chainweb-mining-client
stratum server. All connected clients work together and all mining rewards are
credited to the same account, that is configured in chainweb node.

By default the stratum server sets the difficulty of the work that is sent to
the mining clients to the actual difficulty of the block of the most recent
work. This means that it can take a long time before the client solves any work.
It also means that the target is reset for each new block.

It is also possible to set a custom difficulty. As a consequence not all
accepted solutions qualify as solved blocks, but ASIC miner may provide a more
continuous feedback on its current performance. Some devices may also be more
efficient in this mode an yield higher returns.

The thread count determines how many independent stratum works are used to
concurrently provide work to the clients. Each stratum worker receives one work
header at a time from the chainweb-node mining API and emits jobs for that work
to all clients at a configurable rate. The effective rate of jobs is higher than
then configured rate because when upstream work gets preempted. Each client
receives are all jobs from all workers.

The following command runs two stratum workers that serve jobs at least every 500
milliseconds at a fixed difficulty level of 50.

```
chainweb-mining-client \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:1848 \
    --stratum-port 1917 \
    --stratum-difficulty=50 \
    --stratum-rate=500 \
    --thread-count=2
```

The solution space for each mining job is about 280 terra hashes. The
`--stratum-rate` should be chosen such that the mining devices does not perform
more than that number of hashes within the provided time range. For instance,
for an miner that performs 140TH/s the `--stratum-rate` should be at most 2000 milliseconds
(2 seconds).

The `--stratum-difficulty` parameter expects a integral number between 0 and
256. It denotes the difficulty as a logarithm of base 2. In practice the actual
target uses a difficulty level of at least 42 and at most the difficulty of
block of the current work.

It may help to experiment a little with the `--stratum-difficulty`,
`--stratum-rate`, and the `--thread-count` parameters. We found, that small
values for `--thread-count` (one to three) and moderate values for
`--stratum-difficulty` (in the upper forties / lower fifties) yielded good
results. The But that may differ between different devices and setups.

If the chainweb-node mining API is served via a reverse proxy with TLS the
`--tls` flag must be used to enable HTTPS.

### CPU Mining

Assuming that `example.com` serves the chainweb-node mining API on port 1848, the
following command can be used for CPU mining to the from the previous example:

```sh
chainweb-mining-client \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cfa \
    --node example.com:1848 \
    --worker cpu \
    --thread-count 16 \
```

### GPU Mining

GPU mining is supported via calling an external worker that does the mining
work. An example for such a GPU mining tool for Kadena is
[bigolchunugus](https://github.com/kadena-community/bigolchungus).

```sh
chainweb-mining-client \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:1848 \
    --worker external \
    --external-miner-cmd bigolchunus
```

For selecting the GPU or passing additional arguments to the external tools, the
extra parameters are appended to the command. E.g.

```sh
chainweb-mining-client \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:1848 \
    --worker external \
    --external-worker-cmd "bigolchunus -d 2"
```

The external mining tool is called by adding the target as the last command line
parameter and passing the work bytes on stdin.

### Non-PoW mining

The Non-PoW mining modes ignore the PoW nonce of block headers, instead just
returning the input block header when they "mine" a block. Naturally this makes
their blocks invalid from the perspective of mainnet or testnet. A chainweb
node running in development mode can disable the PoW validity check by setting
the DISABLE_POW_VALIDATION environment variable to `1`, making these modes
produce valid blocks.

The modes are:
    1. simulation. This mode accepts a hash rate via `--hash-rate`, for example
       `--hash-rate 100M` and produces a block with a delay congruent with
       running a real miner at the given hash rate, given the difficulty of the
       block header being mined.

       This mode is useful for testing difficulty adjustment, but because block
       mining times have a long tail, it may be time consuming.

    2. constant-delay. This mode accepts a delay in seconds via
       `--constant-delay-block-time` and produces blocks at a constant rate,
       i.e. a heartbeat, ignoring the block's difficulty.

       This mode is useful for testing that does not involve PoW or difficulty
       adjustment at all.

    3. on-demand. This mode creates an HTTP server that binds to the configured
       interface and port, and listens for a POST request to the `/make-blocks`
       endpoint. The body is a JSON object mapping chain IDs to counts of
       blocks to mine on those chains. The worker will only mine blocks as
       required to mine as many blocks as have been requested and maintain the
       braiding condition of chainweb.

       This mode is useful for generating blocks just after submitting the
       transactions that make them up, or generating empty blocks in bulk to
       get past a certain block height and test some new behavior.

### Creating a Configuration File

The current configuration of chainweb-mining-client can be stored in a
configuration file by adding the `--print-config` parameter

```sh
chainweb-mining-client \
    --public-key 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf \
    --node example.com:1848 \
    --stratum-port 1917 \
    --statum-difficulty=50
    --print-config > config.yml
```

This results in the following configuration file:

```yaml
account: null
externalWorkerCommand: echo 'no external worker command configured' && /bin/false
generateKey: false
hashRate: 1000000.0
insecure: true
logLevel: info
node: example.com:1848
publicKey: 87ef8fdb229ad10285ae191a168ea2ec0794621a127df21e372f41fd0246e4cf
stratumDifficulty: 50
stratumInterface: '*'
stratumPort: 1917
threadCount: 10
useTls: true
worker: stratum
```

The configuration can then be used via

```sh
chainweb-mining-client --config-file config.yml
```

## Related Resources

*   [Chainweb Node Project Page](https://github.com/kadena-io/chainweb-node)
*   [Kadena Stratum Protocol](https://gist.github.com/mightybyte/f1567c2bec0380539c638225fb8c1cf4)
*   [mining API of chainweb-node](https://api.chainweb.com/openapi/#tag/mining)
*   [work header format](https://github.com/kadena-io/chainweb-node/wiki/Block-Header-Binary-Encoding#work-header-binary-format).
*   [mining API wiki](https://github.com/kadena-io/chainweb-node/wiki/Mining-API).
