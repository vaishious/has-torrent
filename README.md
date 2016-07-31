#HasTorrent 
**A library for functional development and testing of BitTorrent algorithms**

##Description

HasTorrent is a BitTorrent implementation written completely in Haskell which define simple yet powerful data types and functions necessary for the creation of a BitTorrent client. Allows the torrent client developer to focus solely on the algorithms he would need for the following decisions:
* When and which tracker requests to issue?
* Which peers to connect to and disconnect from?
* What requests to send to connected peers?
* Which requests received to respond to?

The library defines functions for the following high-level functions:
* Setting up the constant data after checking integrity of .torrent file
* Exchanging data with the tracker
* Exchanging data over tcp to peers through high level messages
* Disk Operations (Setting up files, writing piece to disk)
* Checking Data Integrity (Verifying the checksum of a piece)

##Building

Ensure that `ghc` and `cabal` are installed on your system. Building process done by cabal.

    git clone https://github.com/vaishious/has-torrent
    cd has-torrent
    cabal sandbox init
    cabal install has-torrent

##BitTorrent Extension Protocols (BEP) supported

* [BEP_0003 - The BitTorrent Protocol Specification](http://www.bittorrent.org/beps/bep_0003.html)

This protocol extension encapsulates the crux of the BitTorrent protocol.
Specifies types of messages exchanged between Peers, Bencoding, Tracker request and response protocols and data to file mappings with integrity in mind.
	
* [BEP_0015 - UDP Tracker Protocol for BitTorrent](http://www.bittorrent.org/beps/bep_0015.html)

This extension marks a migration from the HTTP tracker to the UDP tracker to reduce load and the amount of data exchanged by the tracker.
Has become the most commonly used protocol for tracker exchange.

* [BEP_0023 - Tracker Returns Compact Peer Lists](http://www.bittorrent.org/beps/bep_0023.html)

The tracker returns each peer as 32-bit IPv4 addresses and 16-bit port numbers instead of returning them as Bencoded dictionaries. Reduces the amount of bytes exchanged by a factor of at most 40.

##Tools used

Written 100% in Haskell. The list of Haskell dependencies can be found in the [.cabal](https://github.com/vaishious/has-torrent/blob/master/has-torrent.cabal) file.

#####Project Members:
* 13001 - Aadi Yogakar
* 13491 - Pranav Vaish
* 13523 - R Sundararajan
