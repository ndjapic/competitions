program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 26;
var
	i, p: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to N do begin
		read(p);
		write(chr(ord('a') + p-1));
	end;
	readln;
	writeln;
end.
