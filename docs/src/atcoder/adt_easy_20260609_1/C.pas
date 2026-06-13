program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i, p: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	setlength(s, 26);

	for i := 1 to 26 do begin
		read(p);
		s[i] := chr(ord('a') + p-1);
	end;
	readln;

	writeln(s);
end.
