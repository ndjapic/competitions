program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, j, l, r: int32;
	a: array [1 .. nn] of int32;
	p: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	p[1] := 0;
	for i := 1 to n do begin
		read(a[i]);
		p[i] := 0
	end;
	readln;

	for j := 1 to m do begin
		readln(l, r);
		inc(p[l-1]);
		dec(p[r]);
	end;

	for i := 1 to n do begin
		write(p[i-1] * a[i]);
		inc(p[i], p[i-1]);
		if i < n then write(' ');
	end;
	writeln;
end.
