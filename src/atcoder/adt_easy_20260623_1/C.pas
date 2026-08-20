program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, a, b, i, j: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);

	for i := 0 to a*n-1 do begin
		for j := 0 to b*n-1 do
			if odd(i div a + j div b) then
				write('#')
			else
				write('.');
		writeln;
	end;

end.
