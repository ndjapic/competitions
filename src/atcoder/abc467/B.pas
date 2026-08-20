program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a, b, x, y: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	x := 10 * 1000;
	y := 10 * 1000;

	for i := 1 to n do begin
		readln(a, b, s);
		dec(y, a);

		{writeln(s[2]);}
		case s[2] of
			'k': dec(x, b);
			't': dec(x, a);
		end;
	end;

	writeln(y - x);
end.
