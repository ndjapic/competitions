program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100;
var
	n, i, j: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		for j := 1 to n do
			if odd((max(abs(2*i-1 - n), abs(2*j-1 - n)) + n) div 2 + n) then
				write('#')
			else
				write('.');
		writeln;
	end;
end.
