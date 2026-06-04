program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c, d, e, f, g, x, y: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	a := 0;
	b := a+3;
	c := b+1;
	d := c+4;
	e := d+1;
	f := e+5;
	g := f+9;

	case s[1] of
		'A': x := a;
		'B': x := b;
		'C': x := c;
		'D': x := d;
		'E': x := e;
		'F': x := f;
		'G': x := g;
	end;

	case s[3] of
		'A': y := a;
		'B': y := b;
		'C': y := c;
		'D': y := d;
		'E': y := e;
		'F': y := f;
		'G': y := g;
	end;

	writeln(abs(x-y));
end.
