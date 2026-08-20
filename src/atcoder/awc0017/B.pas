program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000 + 1;
var
	n, m, i, j: int32;
	a: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(a[i]);
	readln;

	for j := 1 to m do begin
		read(i);
		inc(a[i-1]);
		inc(a[i]);
		inc(a[i+1]);
	end;
	readln;

	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.
