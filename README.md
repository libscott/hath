# hath

Ethereum transaction utility

```sh
$ hath encodeTx '{"to":null,"from":null,"nonce":0,"value":1234,"to":"","gasPrice":10,"gas":53190,"data":"f4","chainId":16}'
ce800a82cfc6808204d281f4108080
$ !! | hath signTx 3131313131313131313131313131313131313131313131313131313131313131
f84e800a82cfc6808204d281f444a014f469b1b8022b411cbe6e6b19b21578223bb81be0f32048bc258baa905a47d0a037a2c7cc63e8f8b3523700ad23709e2ae7582e62b78b7e6e178c203b5a863e68
$ !! | hath recover
{"addr":"77952ce83ca3cad9f7adcfabeda85bd2f1f52008","pub":"6930f46dd0b16d866d59d1054aa63298b357499cd1862ef16f3f55f1cafceb82f8fcdfbfe88d36edf9cd2d3c99c8d451b3b4a14091da40d00e9333c3d37fe6ab"}
$ hath --help
Usage: hath [--pretty] COMMAND
  Ethereum command line utils

Available options:
  --pretty                 Pretty print output
  -h,--help                Show this help text

Available commands:
  encodeTx                 encode a json transaction
  json                     json api
  signTx                   sign a transaction on stdin
  decodeTx                 decode a transaction on stdin
  serve                    run server
  recover                  recover address
  txid                     get transaction id
```
