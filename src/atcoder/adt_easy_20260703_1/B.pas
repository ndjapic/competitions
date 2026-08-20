program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, o, x: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	o := 0;
	x := 0;
	for i := 1 to n do
		case s[i] of
			'o': inc(o);
			'x': inc(x);
		end;

	if (o > 0) and (x = 0) then
		writeln('Yes')
	else
		writeln('No');
end.
