program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a0, a1: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(a0);
	write(0);

	for i := 1 to n-1 do begin
		read(a1);
		if a1 >= a0 then
			write(' 0')
		else
			write(' ', i);
		a0 := a1;
	end;

	readln;
	writeln;
end.
