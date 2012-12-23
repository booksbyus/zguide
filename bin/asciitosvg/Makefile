all: svg-path.lex svg-path.y
	../lemon-php/lemon -lPHP svg-path.y
	java -cp ../jlexphp/JLexPHP.jar JLexPHP.Main svg-path.lex
	sed -e 's/Yylex/A2S_Yylex/' -e 's/JLexBase/A2S_JLexBase/' < svg-path.lex.php > .tmp.php
	mv .tmp.php svg-path.lex.php
	rm -f .tmp.php
	./mk52.pl < ASCIIToSVG.php > a2s52.php
	php a2s -ilogo.txt -ologo.svg

some:
	./mk52.pl < ASCIIToSVG.php > a2s52.php
	php a2s -ilogo.txt -ologo.svg

clean:
	rm -f svg-path.h svg-path.out

distclean:
	rm -f svg-path.lex.php svg-path.php svg-path.h svg-path.out a2s52.php
