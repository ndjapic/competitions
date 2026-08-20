program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	setlength(s, n);

	for i := 1 to n do
		if i mod 3 > 0 then
			s[i] := 'o'
		else
			s[i] := 'x';

	writeln(s);
end.
