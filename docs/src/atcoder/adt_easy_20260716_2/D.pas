program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	N = 10;
var
	i, j, a, b, c, d: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	a := N;
	b := 1;
	c := N;
	d := 1;

	for i := 1 to N do begin
		readln(s);
		for j := 1 to N do
			if s[j] = '#' then begin
				a := min(a, i);
				b := max(b, i);
				c := min(c, j);
				d := max(d, j);
			end;
	end;

	writeln(a, ' ', b);
	writeln(c, ' ', d);
end.
