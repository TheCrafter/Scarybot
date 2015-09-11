Scarybot
===

Introduction
------------
Scarybot is the IRC bot of Scarybox studios. The main idea is to use the bot to help the visitors of our IRC channel with various ways (such as searching google). But to be honest, the main reason behind this project is to learn Haskell. That's why this project will continue to get updates whenever I learn new techniques.

Right now the bot is only capable of connecting to our irc channel and react to some simple commands. Later, the bot will be able to take commands from channel operators and perform actions such as kicks, bans and statistics.

Features
--------
* Connecting to #scaryboxstudios on freenode and replying to ping and pong messages from freenode.
* Parsing various commsnds and responding to them
  * !gs <search string> -> google search
  * !hs <search string> -> hoogle search
  * !part -> leaves the channel
  * !quit -> shuts down

Building
--------
 1. Clone the project and cd in it.
 2. Run:
 
    ```
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    ```
    
 3. Built binary will reside in the `dist/build/Scarybot` subdirectory.

ChangeLog
---------
 * v0.0.1: Initial release

Contributing
------------
 * For bug fixes, any well checked pull requests are welcome

Credits
-------
Written and maintained by:
* "Vlachakis Dimitris" <dimitrisvlh@gmail.com>

Licensing
---------
Read [LICENSE](LICENSE.md)

Copyright (C) 2015 Vlachakis Dimitris <dimitrisvlh@gmail.com>
All rights reserved.
