program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 8;
var
	i, j: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to N do begin
		readln(s);
		for j := 1 to N do
			if s[j] = '*' then
				write(chr(ord('a') + j-1), 9-i);
	end;
end.
