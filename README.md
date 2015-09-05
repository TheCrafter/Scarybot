Scarybot
===

Introduction
------------
Scarybot is the IRC bot of Scarybox studios.

Features
--------
 * Connecting to #scaryboxstudios on freenode and replying to ping and pong messages from freenode.

Building
--------
 1. Clone the project and cd to the directory of the sample you want to build.
 2. Run:
    ```
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    ```
 3. Built binary will reside in the `dist\build\Scarybot` subdirectory.

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
