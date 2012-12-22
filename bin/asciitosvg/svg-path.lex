<?php # vim:ft=php
include 'jlex.php';
include 'svg-path.php';

%%
%{
	/* w/e */
%}

%function nextToken
D	= [0-9]
E   = [Ee][+-]?{D}+

%%

<YYINITIAL> [0|1]		{ return $this->createToken(A2S_SVGPathParser::TK_FLAG); }
<YYINITIAL> [+]?{D}+	{ return $this->createToken(A2S_SVGPathParser::TK_POSNUM); }
<YYINITIAL> "-"{D}+ 	{ return $this->createToken(A2S_SVGPathParser::TK_POSNUM); }
<YYINITIAL> [+]?{D}*"."{D}+({E})?	{ return $this->createToken(A2S_SVGPathParser::TK_POSNUM); }
<YYINITIAL> [+]?{D}+"."{D}*({E})?	{ return $this->createToken(A2S_SVGPathParser::TK_POSNUM); }
<YYINITIAL> "-"{D}*"."{D}+({E})?	{ return $this->createToken(A2S_SVGPathParser::TK_POSNUM); }
<YYINITIAL> "-"{D}+"."{D}*({E})?	{ return $this->createToken(A2S_SVGPathParser::TK_POSNUM); }
<YYINITIAL> [M|m]	{ return $this->createToken(A2S_SVGPathParser::TK_MCMD); }
<YYINITIAL> [Z|z]	{ return $this->createToken(A2S_SVGPathParser::TK_ZCMD); }
<YYINITIAL> [L|l]	{ return $this->createToken(A2S_SVGPathParser::TK_LCMD); }
<YYINITIAL> [H|h]	{ return $this->createToken(A2S_SVGPathParser::TK_HCMD); }
<YYINITIAL> [V|v]	{ return $this->createToken(A2S_SVGPathParser::TK_VCMD); }
<YYINITIAL> [Q|q]	{ return $this->createToken(A2S_SVGPathParser::TK_QCMD); }
<YYINITIAL> [C|c]	{ return $this->createToken(A2S_SVGPathParser::TK_CCMD); }
<YYINITIAL> [S|s]	{ return $this->createToken(A2S_SVGPathParser::TK_SCMD); }
<YYINITIAL> [T|t]	{ return $this->createToken(A2S_SVGPathParser::TK_TCMD); }
<YYINITIAL> [A|a]	{ return $this->createToken(A2S_SVGPathParser::TK_ACMD); }
<YYINITIAL> [ ,\t\v\n\f\r] { }
. { }
