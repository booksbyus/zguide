%name A2S_SVGPath
%token_prefix TK_
%wildcard ANY.

svg_path ::= moveto_drawto_command_groups.

moveto_drawto_command_groups ::= moveto_drawto_command_groups moveto_drawto_command_group.
moveto_drawto_command_groups ::= moveto_drawto_command_group.

moveto_drawto_command_group ::= moveto drawto_commands.

drawto_commands ::= drawto_commands drawto_command.
drawto_commands ::= drawto_command.

drawto_command ::= closepath.
drawto_command ::= lineto.
drawto_command ::= horizontal_lineto.
drawto_command ::= vertical_lineto.
drawto_command ::= curveto.
drawto_command ::= smooth_curveto.
drawto_command ::= quadratic_bezier_curveto.
drawto_command ::= smooth_quadratic_bezier_curveto.
drawto_command ::= elliptical_arc.

moveto ::= MCMD(A) moveto_argument_sequence(B).
	{
		if (count(B) == 2) {
			$this->commands[] = array_merge(array(A), B);
		} else { 
			if (A->value == 'm') {
				$arr = array ('value' => 'l');
			} else {
				$arr = array ('value' => 'L');
			}
			$c = array_splice(B, 2);
			$this->commands[] = array_merge(array(A), B);
			$this->commands[] = array_merge(array($arr), $c);
		}
	}

moveto_argument_sequence(A) ::= lineto_argument_sequence(B)
	coordinate_pair(C). { A = array_merge(B, C); }
moveto_argument_sequence(A) ::= coordinate_pair(B). { A = B; }

closepath ::= ZCMD(A). { $this->commands[] = array(A); }

lineto ::= LCMD(A) lineto_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

lineto_argument_sequence(A) ::= lineto_argument_sequence(B) coordinate_pair(C).
	{ A = array_merge(B, C); }
lineto_argument_sequence(A) ::= coordinate_pair(B). { A = B; }

horizontal_lineto ::= HCMD(A) horizontal_lineto_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

horizontal_lineto_argument_sequence(A) ::=
	horizontal_lineto_argument_sequence(B) coordinate(C).
	{ A = array_merge(B, array(C)); }
horizontal_lineto_argument_sequence(A) ::= coordinate(B). { A = array(B); }

vertical_lineto ::= VCMD(A) vertical_lineto_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

vertical_lineto_argument_sequence(A) ::= vertical_lineto_argument_sequence(B)
	coordinate(C).
	{ A = array_merge(B, array(C)); }
vertical_lineto_argument_sequence(A) ::= coordinate(B). { A = array(B); }

curveto ::= CCMD(A) curveto_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

curveto_argument_sequence(A) ::= curveto_argument_sequence(B)
	curveto_argument(C).
	{ A = array_merge(B, C); }
curveto_argument_sequence(A) ::= curveto_argument(B). { A = B; }

curveto_argument(A) ::= coordinate_pair(B) coordinate_pair(C)
	coordinate_pair(D).
	{ A = array_merge(B, C, D); }

smooth_curveto ::= SCMD(A) smooth_curveto_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

smooth_curveto_argument_sequence(A) ::= smooth_curveto_argument_sequence(B)
	smooth_curveto_argument(C).
	{ A = array_merge(B, C); }
smooth_curveto_argument_sequence(A) ::= smooth_curveto_argument(B). { A = B; }

smooth_curveto_argument(A) ::= coordinate_pair(B) coordinate_pair(C).
	{ A = array_merge(B, C); }

quadratic_bezier_curveto ::= QCMD(A)
	quadratic_bezier_curveto_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

quadratic_bezier_curveto_argument_sequence(A) ::=
	quadratic_bezier_curveto_argument_sequence(B)
	quadratic_bezier_curveto_argument(C).
	{ A = array_merge(B, C); }

quadratic_bezier_curveto_argument_sequence(A) ::=
	quadratic_bezier_curveto_argument(B).
	{ A = B; }

quadratic_bezier_curveto_argument(A) ::= coordinate_pair(B) coordinate_pair(C).
	{ A = array_merge(B, C); }

smooth_quadratic_bezier_curveto ::= TCMD(A)
	smooth_quadratic_bezier_curveto_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

smooth_quadratic_bezier_curveto_argument_sequence(A) ::=
	smooth_quadratic_bezier_curveto_argument_sequence(B)
	coordinate_pair(C).
	{ A = array_merge(B, C); }
smooth_quadratic_bezier_curveto_argument_sequence(A) ::= coordinate_pair(B).
	{ A = B; }

elliptical_arc ::= ACMD(A) elliptical_arc_argument_sequence(B).
	{ $this->commands[] = array_merge(array(A), B); }

elliptical_arc_argument_sequence(A) ::= elliptical_arc_argument_sequence(B)
	elliptical_arc_argument(C).
	{ A = array_merge(B, C); }
elliptical_arc_argument_sequence(A) ::= elliptical_arc_argument(B).
	{ A = B; }

elliptical_arc_argument(A) ::= POSNUM(B) POSNUM(C) number(D) FLAG(E) FLAG(F)
	coordinate_pair(G).
	{ A = array_merge(array(B, C, D, E, F), G); }

coordinate_pair(A) ::= coordinate(B) coordinate(C). { A = array(B, C); }

coordinate(A) ::= number(B). { A = B; }

number(A) ::= POSNUM(B). { A = B; }
number(A) ::= FLAG(B). { A = B; }
number(A) ::= NEGNUM(B). { A = B; }

