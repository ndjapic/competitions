program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, q, i, l, r, c: int32;
	a: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for r := 0 to n do a[r] := 0;

	for i := 1 to q do begin
		readln(l, r, c);
		inc(a[l-1], c);
		dec(a[r], c);
	end;

	for r := 1 to n do begin
		write(a[r-1]);
		if r < n then write(' ');
		inc(a[r], a[r-1]);
	end;
	writeln;
end.
