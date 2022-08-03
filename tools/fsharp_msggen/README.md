## F# type generator for C-Lightning RPC.

c-lightning has a tool for parsing its json-schema files
and generate arbitrary file from it, called `msggen`

We use this feature to generate F# DTOs and client methods.
To run this project,
1. install poetry and python of appropriate version,
2. clone DNL repository with submodule (c-lightning)
3. And run `poetry run msggen` in this repository.

We do not run this automatically for the security purpose.
Rather we require new PR to make a change to the DTOs.
