program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a, b, c, d: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	a := 0;
	b := 0;
	c := 0;
	d := 0;

	for i := 1 to n do
		if s[i] = 'C' then inc(c);

	for i := 1 to n do
		case s[i] of
			'A': inc(a);
			'B': if (a > 0) and (c > 0) then begin
				dec(a);
				inc(b);
				dec(c);
				inc(d);
			end;
			'C': if d > 0 then
				dec(d)
			else
				dec(c);
		end;

	writeln(b);
end.
