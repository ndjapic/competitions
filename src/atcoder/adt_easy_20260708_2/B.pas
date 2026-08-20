program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, h: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	setlength(s, n);
	for i := 1 to n do s[i] := '-';

	h := (n+1) div 2;
	s[h] := '=';
	s[n+1-h] := '=';

	writeln(s);
end.
