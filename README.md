# hath

Ethereum transaction utility for offline signing.

Not very polished, probably never will be, might be good reference material though.

## Installation

```
$ curl -sSL https://get.haskellstack.org/ | sudo sh
[...] Installed haskell-stack
$ git clone https://github.com/libscott/hath
$ cd hath
$ stack install
[...]
Copied executables to /home/scott/.local/bin:
- hath
```

## Usage

```sh
$ hath --help
Usage: hath COMMAND
  Ethereum command line utils

Available options:
  -h,--help                Show this help text

Available commands:
  encodeTx                 encode a json transaction
  signTx                   sign a transaction on stdin
  decodeTx                 decode a transaction on stdin
  serve                    run server
  recover                  recover address
  txid                     get transaction id

$ hath encodeTx '{ "to":null, "from":null, "nonce":0, "value":1234,
                   "gasPrice":10, "gas":53190,"data":"f4","chainId":16 }'
ce800a82cfc6808204d281f4108080

$ !! | hath signTx 3131313131313131313131313131313131313131313131313131313131313131
f84e800a82cfc6808204d281f444a014f469b1b8022b411cbe6e6b19b21578223bb81be0f32048bc258 [...]

$ !! | hath recover
{"addr":"77952ce83ca3cad9f7adcfabeda85bd2f1f52008","pub":"6930f46d866 [...] 0d3c3d37fe6ab"}
```
