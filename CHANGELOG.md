# Revision history for chainweb-mining-client

## 0.5 -- 2022-11-23

*   Add `--stratum-rate` option, which specifies the mining rate (in
    milliseconds) at which each worker thread emits new mining jobs to the
    client.

*   Change default value for thread count to 2.
*   Change default to connect to chainweb-node over an unsecure connection.
*   Change default to require valid certificates when TLS is enabled.
*   Change default chainweb-node host address to use port 1848.

## 0.4 -- 2021-11-29

*   Add `--account` command line option for specifying the mining account. The
    default value is public miner key prefix with `:k` (k-account).

## 0.3 -- 2021-11-10

*   Add stratum server (#9)

## 0.2 -- 2020-08-20

* Rename package into chainweb-mining-client

## 0.1 -- 2020-08-20

* First version. Released on an unsuspecting world.
