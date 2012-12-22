#!/usr/bin/env perl
# vim:ts=2:sw=2:et:
# Converts a PHP file with namespaces to a PHP 5.2 compatible form.
# Reads a PHP file from stdin and emits the "fixed" version on stdout
use strict;

local $/ = undef;
my $src = <>;

# Comment out namespace specification
$src =~ s/^namespace/#namespace/smg;


# Find all class names, but ignore "class" in comments
my $nocomments = $src;
$nocomments =~ s,/\*.*?\*/,,smg;
# For each class, fix the namespace up to a class prefix
while ($nocomments =~ m/class\s+(\S+)/g) {
  my $class = $1;
  $src =~ s/\b$class\b/A2S_$class/g;
}

# Get rid of \A2S namespace usage for scanner / parser.
$src =~ s/\\A2S_SVGPathParser/A2S_SVGPathParser/g;
$src =~ s/\\A2S_Yylex/A2S_Yylex/g;

# Not perfect, since it changes some things in comments in generated output
print $src;

