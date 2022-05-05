---
title: DotNetLightning.* namespaces
category: overview
index: 1
categoryindex: 1
---

# Overview of the features in "DotNetLightning.*" sub-namespaces.

## DotNetLightning.Utils

Contains a low-level primitives for LN.
Mostly it is for internal usage but some are useful for consumer's point of view.
(e.g. LNMoney to represent milli-satoshis value)

## DotNetLightning.Serialization

Contains items for wire-protocol. FeatureBits, TLV, and P2P messages.

## DotNetLightning.Crypto

Contains modules and types for working with Cryptographic operations.
For example LN-onion network encoding, [aezeed](https://github.com/lightningnetwork/lnd/tree/master/aezeed) for seed
backups.

## DotNetLightning.Transactions

A module for creating LN-specific transactions. Mostly for internal usage.

## DotNetLightning.Peer

Handles handshake and encryption against other peers.

## DotNetLightning.Channel

Handles channel state.
This module is pretty much WIP (not sure when it will be finished, as this is the most complex part in the LN protocol).

## DotNetLightning.Payment

Contains primitives for Payment-related operations. The most important class is `PaymentRequest`,
a.k.a. bolt11-invoice, LN-invoice.

It also contains primitives for [LSAT](https://github.com/lightninglabs/LSATI), the LN based http authentication mechanism.
See [here](https://github.com/joemphilips/LSATAuthentication) for PoC of AspNetCore middleware for LSAT.

## DotNetLightning.Routing

Module for calculating payment routes. (WIP)
