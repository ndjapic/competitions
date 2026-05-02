program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, c, q, d: int32;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	c := 0;
	q := 0;

	for i := 1 to n do begin
		readln(ch);
		case ch of
			'R': inc(c);
			'W': dec(c);
			'?': inc(q);
		end;
	end;

	c := abs(c);
	d := min(c, q);
	dec(c, d);
	dec(q, d);
	inc(c, q mod 2);

	writeln(c);
end.
