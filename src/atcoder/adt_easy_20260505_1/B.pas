program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, d: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	d := 0;
	for i := 1 to n do
		case s[i] of
			'T': dec(d);
			'A': inc(d);
		end;

	if d < 0 then
		writeln('T')
	else if d > 0 then
		writeln('A')
	else if s[n] = 'T' then
		writeln('A')
	else if s[n] = 'A' then
		writeln('T');
end.
