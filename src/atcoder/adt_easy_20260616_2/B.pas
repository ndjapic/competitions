program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	l, m, i: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	l := length(s);
	m := (l+1) div 2;

	for i := 1 to m-1 do write(s[i]);
	for i := m+1 to l do write(s[i]);
	writeln;
end.
