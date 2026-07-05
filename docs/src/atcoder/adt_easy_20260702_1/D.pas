program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unfinished
const
	NN = 100;
var
	n, m, i, x: int8;
	a, c: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for x := 1 to m do c[x] := 0;

	for i := 1 to n do begin
		read(x);
		a[i] := x;
		inc(c[x]);
	end;
	readln;

	x := 1;
	while (x <= m) and (c[x] > 0) do inc(x);

	i := n;
	if x > m then begin
		x := a[i];
		while (i > 0) and (c[x] > 1) do begin
			dec(i);
			x := a[i];
		end;
		dec(c[x]);
	end;

	writeln(n+1-i);
end.
