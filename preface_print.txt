+ Preface

++ ZeroMQ in a Hundred Words

ZeroMQ (also known as 0MQ, 0\MQ, or zmq) looks like an embeddable networking library but acts like a concurrency framework. It gives you sockets that carry atomic messages across various transports like in-process, inter-process, TCP, and multicast. You can connect sockets N-to-N with patterns like fan-out, pub-sub, task distribution, and request-reply. It's fast enough to be the fabric for clustered products. Its asynchronous I/O model gives you scalable multicore applications, built as asynchronous message-processing tasks. It has a score of language APIs and runs on most operating systems. ZeroMQ is from [http://www.imatix.com iMatix] and is LGPLv3 open source.

++ How It Began

We took a normal TCP socket, injected it with a mix of radioactive isotopes stolen from a secret Soviet atomic research project, bombarded it with 1950-era cosmic rays, and put it into the hands of a drug-addled comic book author with a badly-disguised fetish for bulging muscles clad in spandex[figure]. Yes, ZeroMQ sockets are the world-saving superheroes of the networking world.

++ The Zen of Zero

[[code type="textdiagram" title="A terrible accident..."]]
.------------.        .-------------.
|            |        |             |
| TCP socket +------->|   ZeroMQ    | ZAP!
|            | BOOM!  |             |
'------------'        |   Socket    |  POW!!
  ^    ^    ^         |             |
  |    |    |         '-------------'
  |    |    |
  |    |    |
  |    |    '--------- Spandex
  |    |
  |    '-------------- Cosmic rays

 Illegal radioisotopes from
 secret Soviet atomic city
[[/code]]

The Ø in ZeroMQ is all about tradeoffs. On the one hand this strange name lowers ZeroMQ's visibility on Google and Twitter. On the other hand it annoys the heck out of some Danish folk who write us things like "ØMG røtfl", and "Ø is not a funny looking zero!" and "//Rødgrød med fløde!//", which is apparently an insult that means "may your neighbours be the direct descendants of Grendel!"  Seems like a fair trade.

Originally the zero in ZeroMQ was meant as "zero broker" and (as close to) "zero latency" (as possible). Since then, it has come to encompass different goals: zero administration, zero cost, zero waste. More generally, "zero" refers to the culture of minimalism that permeates the project. We add power by removing complexity rather than by exposing new functionality.

++ How This Book Came To Be

In the summer of 2010, ZeroMQ was still a little-known niche library described by its rather terse reference manual and a living but sparse wiki. Martin Sustrik and myself were sitting in the bar of the Hotel Kyjev in Bratislava plotting how to make ZeroMQ more widely popular. Martin had written most of the ZeroMQ code, and I'd put up the funding and organized the community. Over some Zlaty Bazants, we agreed that ZeroMQ needed a new, simpler web site and a basic guide for new users.

Martin collected some ideas for topics to explain. I'd never written a line of ZeroMQ code before this, so it became a live learning documentary. As I worked through simple examples to more complex ones, I tried to answer many of the questions I'd seen on the mailing list. Because I'd been building large-scale architectures for 30 years, there were a lot of problems at which I was keen to throw ZeroMQ. Amazingly the results were mostly simple and elegant, even when working in C. I felt a pure joy learning ZeroMQ and using it to solve real problems, which brought me back to programming after a few years' pause. And often, not knowing how it was "supposed" to be done, we improved ZeroMQ as we went along.

From the start, I wanted the ZeroMQ guide to be a community project, so I put it onto GitHub and let others contribute with pull requests. This was considered a radical, even vulgar approach by some. We came to a division of labor: I'd do the writing and make the original C examples, and others would help fix the text and translate the examples into other languages.

This worked better than I dared hope. You can now find all the examples in several languages and many in a dozen languages. It's a kind of programming language Rosetta stone and a valuable outcome in itself. We set up a high score: reach 80% translation and your language got its own Guide. PHP, Python, Lua, and Haxe reached this goal. People asked for PDFs, and we created those. People asked for ebooks, and got those. About a hundred people contributed to the examples to date.

The book, in its on-line version "The Guide", achieved its goal of popularizing ZeroMQ. The style pleases most and annoys some, which is how it should be. In December 2010, my work on ZeroMQ and this guide stopped, as I found myself going through late-stage cancer, heavy surgery, and six months of chemotherapy. When I picked up work again in mid-2011, it was to start using ZeroMQ in anger for one of the largest use cases imaginable: on the mobile phones and tablets of the world's biggest electronics company.

But the goal of the ZeroMQ book was, from the start, a printed work. So it was exciting to get an email from Bill Lubanovic in January 2012 introducing me to his editor, Andy Oram, at O'Reilly, suggesting a ZeroMQ book. Of course! Where do I sign? How much do I have to pay? Oh, I //get money// for this? All I have to do is finish it?

Of course as soon as O'Reilly announced a ZeroMQ book, other publishers started sending out emails to potential authors. You'll probably see a rash of ZeroMQ books coming out next year. That's good. Our niche library has hit the mainstream and deserves its six inches of shelf space. My apologies to the other ZeroMQ authors. We've set the bar horribly high, and my advice is to make your books complementary. Perhaps focus on a specific language, platform, or pattern.

This is the magic and power of communities: be the first community in a space, stay healthy, and you own that space for ever.

++ Audience

This book is written for professional programmers who want to learn how to make the massively distributed software that will dominate the future of computing. We assume you can read C code, because most of the examples here are in C even though ZeroMQ is used in many languages. We assume you care about scale, because ZeroMQ solves that problem above all others. We assume you need the best possible results with the least possible cost, because otherwise you won't appreciate the trade-offs that ZeroMQ makes. Other than that basic background, we try to present all the concepts in networking and distributed computing you will need to use ZeroMQ.

.inbook preface.xml
