Welcome
-------

The Guide is written by Pieter Hintjens, with help from Martin Sustrik, Gonzalo
Diethelm, Zed Shaw, CAF, Guido Goldstein, Oliver Smith,  Pierre Rouleau, Peter
Alexander, and others.  We cover 0MQ version 2.x.  Add errata and suggestions to
http://www.zeromq.org/docs:user-guide-talk or as patches off this git emailed to
the zeromq-dev list.

To build the Guide you need Ditaa (included in this repository), ImageMagick, perl.
Run the command: "z2w chapter*.txt".  The result is in wdtemp.txt.

Currently, formats wdtemp.txt ready for pasting into a page at www.zeromq.org.
Images and source examples are hosted here at github.com and to modify an image
or example we commit it to this repository.

Examples
--------

The examples from the 0MQ Guide help people to learn and use 0MQ.  While we
wrote most of the examples in C, we welcome translations into other languages.
All example code is licensed under MIT/X11.

Please send your translations, and fixes as patches to the zeromq-dev list
with the subject line "[PATCH] zguide: <filename>". We'll review the code and
add it to the zguide repository if it's ok.

Patches are the way we accept contributions.  It ensures that more than one
person can work on code at the same time.

The best way to submit patches is to clone this repository, make your changes,
and use git to create a patch.  Google "make git patch" for lots of examples.

If you don't want to use git, you can send us entire examples but only if the
code was empty before.  If you want to update or change an example you MUST
use git patches.

Guidelines
----------

Please:

* Stick to identical functionality and naming used in examples so that readers
  can easily compare languages.
* You MUST place your name as author in the examples so readers can contact you.
* You MUST state in the email that you license your code under the MIT/X11
  license.

Thanks!

Resources
---------

* http://en.wikipedia.org/wiki/MIT_License.
* http://www.gimp.org/bugs/howtos/submit-patch.html
