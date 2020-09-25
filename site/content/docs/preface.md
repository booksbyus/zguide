---
weight: 0
---


**By Pieter Hintjens, CEO of iMatix**

Please use the [issue tracker](https://github.com/booksbyus/zguide/issues) for all comments and errata. This version covers the latest stable release of ZeroMQ (3.2). If you are using older versions of ZeroMQ then some of the examples and explanations won't be accurate.

The Guide is originally [in C](/page:all), but also in [PHP](/php:all), [Java](/java:all), [Python](/py:all), [Lua](/lua:all), and [Haxe](/hx:all). We've also translated most of the examples into C++, C#, CL, Delphi, Erlang, F#, Felix, Haskell, Julia, Objective-C, Ruby, Ada, Basic, Clojure, Go, Haxe, Node.js, ooc, Perl, and Scala.

# Preface

## ZeroMQ in a Hundred Words {#ZeroMQ-in-a-Hundred-Words}

ZeroMQ (also known as ØMQ, 0MQ, or zmq) looks like an embeddable networking library but acts like a concurrency framework. It gives you sockets that carry atomic messages across various transports like in-process, inter-process, TCP, and multicast. You can connect sockets N-to-N with patterns like fan-out, pub-sub, task distribution, and request-reply. It's fast enough to be the fabric for clustered products. Its asynchronous I/O model gives you scalable multicore applications, built as asynchronous message-processing tasks. It has a score of language APIs and runs on most operating systems. ZeroMQ is from [iMatix](http://www.imatix.com) and is LGPLv3 open source.

## How It Began {#How-It-Began}

We took a normal TCP socket, injected it with a mix of radioactive isotopes stolen from a secret Soviet atomic research project, bombarded it with 1950-era cosmic rays, and put it into the hands of a drug-addled comic book author with a badly-disguised fetish for bulging muscles clad in spandex. Yes, ZeroMQ sockets are the world-saving superheroes of the networking world.

{{< textdiagram name="fig1.png" figno="1" title="A terrible accident..." >}}
.------------.        .------------.
|            |        |            |
| TCP socket +------->|   ZeroMQ   | ZAP!
|            | BOOM!  |            |
'------------'        |   Socket   |  POW!!
  ^    ^    ^         |            |
  |    |    |         '------------'
  |    |    |
  |    |    |
  |    |    '--------- Spandex
  |    |
  |    '-------------- Cosmic rays

 Illegal radioisotopes from
 secret Soviet atomic city
{{< /textdiagram >}}

## The Zen of Zero {#The-Zen-of-Zero}

The Ø in ZeroMQ is all about tradeoffs. On the one hand this strange name lowers ZeroMQ's visibility on Google and Twitter. On the other hand it annoys the heck out of some Danish folk who write us things like "ØMG røtfl", and "Ø is not a funny looking zero!" and "*Rødgrød med fløde!*", which is apparently an insult that means "may your neighbours be the direct descendants of Grendel!"  Seems like a fair trade.

Originally the zero in ZeroMQ was meant as "zero broker" and (as close to) "zero latency" (as possible). Since then, it has come to encompass different goals: zero administration, zero cost, zero waste. More generally, "zero" refers to the culture of minimalism that permeates the project. We add power by removing complexity rather than by exposing new functionality.

## Audience {#Audience}

This book is written for professional programmers who want to learn how to make the massively distributed software that will dominate the future of computing. We assume you can read C code, because most of the examples here are in C even though ZeroMQ is used in many languages. We assume you care about scale, because ZeroMQ solves that problem above all others. We assume you need the best possible results with the least possible cost, because otherwise you won't appreciate the trade-offs that ZeroMQ makes. Other than that basic background, we try to present all the concepts in networking and distributed computing you will need to use ZeroMQ.

## Acknowledgements {#Acknowledgements}

Thanks to Andy Oram for making [the O'Reilly book](http://shop.oreilly.com/product/0636920026136.do) happen, and editing this text.

Thanks to Bill Desmarais, Brian Dorsey, Daniel Lin, Eric Desgranges, Gonzalo Diethelm, Guido Goldstein, Hunter Ford, Kamil Shakirov, Martin Sustrik, Mike Castleman, Naveen Chawla, Nicola Peduzzi, Oliver Smith, Olivier Chamoux, Peter Alexander, Pierre Rouleau, Randy Dryburgh, John Unwin, Alex Thomas, Mihail Minkov, Jeremy Avnet, Michael Compton, Kamil Kisiel, Mark Kharitonov, Guillaume Aubert, Ian Barber, Mike Sheridan, Faruk Akgul, Oleg Sidorov, Lev Givon, Allister MacLeod, Alexander D'Archangel, Andreas Hoelzlwimmer, Han Holl, Robert G. Jakabosky, Felipe Cruz, Marcus McCurdy, Mikhail Kulemin, Dr. Gergő Érdi, Pavel Zhukov, Alexander Else, Giovanni Ruggiero, Rick "Technoweenie", Daniel Lundin, Dave Hoover, Simon Jefford, Benjamin Peterson, Justin Case, Devon Weller, Richard Smith, Alexander Morland, Wadim Grasza, Michael Jakl, Uwe Dauernheim, Sebastian Nowicki, Simone Deponti, Aaron Raddon, Dan Colish, Markus Schirp, Benoit Larroque, Jonathan Palardy, Isaiah Peng, Arkadiusz Orzechowski, Umut Aydin, Matthew Horsfall, Jeremy W. Sherman, Eric Pugh, Tyler Sellon, John E. Vincent, Pavel Mitin, Min RK, Igor Wiedler, Olof Åkesson, Patrick Lucas, Heow Goodman, Senthil Palanisami, John Gallagher, Tomas Roos, Stephen McQuay, Erik Allik, Arnaud Cogoluègnes, Rob Gagnon, Dan Williams, Edward Smith, James Tucker, Kristian Kristensen, Vadim Shalts, Martin Trojer, Tom van Leeuwen, Hiten Pandya, Harm Aarts, Marc Harter, Iskren Ivov Chernev, Jay Han, Sonia Hamilton, Nathan Stocks, Naveen Palli, and Zed Shaw for their contributions to this work.

