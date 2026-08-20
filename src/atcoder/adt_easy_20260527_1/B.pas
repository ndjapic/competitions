program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	setlength(s, n+3);

	s[1] := 'L';
	for i := 2 to n+1 do s[i] := 'o';
	s[n+2] := 'n';
	s[n+3] := 'g';

	writeln(s);
end.
