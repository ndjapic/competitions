program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, l, x, y: int32;
	a: array [1 .. NN] of array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 1 to n do begin
		read(l);
		setlength(a[x], l);
		for y := 0 to l-1 do read(a[x][y]);
		readln;
	end;

	readln(x, y);
	writeln(a[x][y-1]);
end.
