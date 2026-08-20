program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, f, a: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	f := 0;
	a := 0;

	for i := 1 to n do begin
		readln(s);
		case s[1] of
			'F': inc(f);
			'A': inc(a);
		end;
	end;

	if f > a then
		writeln('Yes')
	else
		writeln('No');
end.
