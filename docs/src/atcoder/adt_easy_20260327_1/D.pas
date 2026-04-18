program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, x: int8;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	read(a[1]);
	for i := 2 to n do begin
		read(a[i]);
		if a[i-1] < a[i] then
			for x := a[i-1] to a[i]-1 do write(x, ' ')
		else if a[i-1] > a[i] then
			for x := a[i-1] downto a[i]+1 do write(x, ' ');
	end;
	readln;
	writeln(a[n]);
end.
