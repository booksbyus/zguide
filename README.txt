.set GIT=http://github.com/imatix/zguide
.sub 0MQ=ØMQ

0MQ - The Guide
===============

Written by Pieter Hintjens <ph@imatix.com>, CEO iMatix Corporation.

A 0MQ socket is what you get when you take a normal TCP socket, inject it with a mix of radioactive isotopes stolen from a secret Soviet atomic research project, bombard it with 1950-era cosmic rays, and put it into the hands of a drug-addled comic book author with a badly-disguised fetish for bulging muscles clad in spandex.  Yes, 0MQ sockets are the world-saving superheros of the networking world.

[diagram]
 +------------+        +------------+
 |            |        |            | Zap!
 | TCP socket +------->| 0MQ socket |
 |            | BOOM!  |     cC00   |  POW!!
 +------------+        +------------+
   ^    ^    ^
   |    |    |
   |    |    \---------\
   |    |              |
   |    \----------\   |
  Illegal          |   |
  radioisotopes    |   |
  from secret      |   |
  Soviet atomic    | Spandex
  city             |
               Cosmic rays


        A terrible accident...
[/diagram]

Contents
--------

.toc 1

License
-------

The text of "0MQ - The Guide" is copyright (c) 2010 Pieter Hintjens, and is licensed under the Creative Commons Attribution-Share Alike 3.0 License.  The source code examples are licensed under MIT/X11.  `z2w` is placed into the public domain.

Thanks
------

Thanks to Bill Desmarais, Brian Dorsey, CAF, Daniel Lin, Eric Desgranges, Gonzalo Diethelm, Guido Goldstein, Hunter Ford, Kamil Shakirov, Martin Sustrik, Mike Castleman, Naveen Chawla, Nicola Peduzzi, Oliver Smith, Olivier Chamoux, Peter Alexander, Pierre Rouleau, Randy Dryburgh, and Zed Shaw for their contributions, and to Stathis Sideris for [Ditaa](ditaa.org).

General
-------

The Guide is a general introduction to 0MQ, and covers version 2.0.x.  It will be updated to cover 2.1.0 as soon as that is formally released.

To submit an errata use the [issue tracker](http://github.com/imatix/zguide/issues).  All discussion of the contents or examples happens on the zeromq-dev list or #zeromq IRC channel.

Examples
--------

The examples from the Guide help people to learn and use 0MQ.  While we wrote most of the examples in C, we welcome translations into other languages. All example code is licensed under MIT/X11.

Please send your translations, and fixes as patches to the zeromq-dev list with the subject line "[PATCH] zguide: <filename>". We'll review the code and add it to the zguide repository if it's ok.

Patches are the way we accept contributions.  It ensures that more than one person can work on code at the same time.

The best way to submit patches is to clone this repository, make your changes, and use git to create a patch.  See http://www.zeromq.org/docs:contributing.

If you don't want to use git, you can send us entire examples but only if the code was empty before.  If you want to update or change an example you MUST use git patches.

Guidelines
----------

Please:

* Stick to identical functionality and naming used in examples so that readers can easily compare languages.
* You MUST place your name as author in the examples so readers can contact you.
* You MUST state in the email that you license your code under the MIT/X11 license, or else use git's signoff feature as explained on this page: http://www.zeromq.org/docs:contributing

Thanks!

Build Process
-------------

To rebuild the Guide from this git repository you need z2w and Ditaa (included in the bin directory), ImageMagick, perl. Run the command: "z2w chapter*.txt".  The result is a series of files named `chapter1.wd`, `chapter2.wd`, etc.  Paste these into a Wikidot site appropriately.

Images and source examples are hosted here at github.com and to modify an image or example we commit it to this repository.

This document is originally at README.txt and is built using [gitdown](http://github.com/imatix/gitdown), a great little tool by the same author.
