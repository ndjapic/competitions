program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	k, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k);
	setlength(s, k);

	for i := 1 to k do
		s[i] := chr(ord('A') + i-1);

	writeln(s);
end.
