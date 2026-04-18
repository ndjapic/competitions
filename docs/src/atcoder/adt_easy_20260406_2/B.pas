program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	a, b: int16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(a);

	for i := 1 to n-1 do begin
		read(b);
		write(a * b);
		if i < n-1 then write(' ');
		a := b;
	end;
	readln;
	writeln;
end.
