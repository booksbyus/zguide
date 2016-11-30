# Dependencies

Tested on Debian 8.6.0.

```sh
sudo apt-get install dblatex dbtoepub ditaa doc-base \
  docbook docbook-dsssl docbook-utils docbook-xml docbook-xsl \
  ghostscript libbatik-java pstoedit perl \
  python python-pygments 
```

On Ubuntu, you need (at least) these packages:

    asciitosvg # install into /usr/local/bin
    dbtoepub
    docbook2ps
    inkscape
    ps2pdf
    pstoedit
    pygmentize
    rasterizer

# Update Wikidot

Set these environment variables if you want to update the Wikidot copies of the
newly generated book:

```sh
set APISITE_USER=[your-wikidot-username]
set APISITE_KEY=[your-wikidot-API-access-key]
```

# Build PDF and ePUB files

It is important to run this script from the root of this repository.

```sh
./bin/buildpdfs
```

The newly generated PDFs and ePUBs will be in the root directory of this 
repository:

```
opedroso@OPLIN:~/git/zguide$ ls *.pdf *.epub
zguide-c.pdf  zguide-hx.pdf  zguide-lua.pdf  zguide-php.pdf  zguide-py.pdf
zguide-c.epub  zguide-hx.epub  zguide-lua.epub  zguide-php.epub  zguide-py.epub
```
