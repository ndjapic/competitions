program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, j: int8;
	s1, s2: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	setlength(s1, w);
	setlength(s2, w);

	for j := 1 to w do begin
		s1[j] := '#';
		s2[j] := '.';
	end;

	s2[1] := '#';
	s2[w] := '#';

	writeln(s1);
	for i := 2 to h-1 do writeln(s2);
	writeln(s1);
end.
