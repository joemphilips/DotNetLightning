

## Things what we are not going to do

* Support .NET Framework ... Since we heavily rely on `Span<T>` for binary serialization.

## TODO

* Refactor PeerChannelEncryptor to optimize performance (Probably by using byref-like-type and mutable state)
* Update `Channel.deriveOurDustLimitSatoshis`
