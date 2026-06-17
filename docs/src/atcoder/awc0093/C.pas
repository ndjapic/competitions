program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, m, i, j, l, r, d: int32;
	a, b: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		read(a[i]);
		b[i] := 0;
	end;
	readln;

	for j := 1 to m do begin
		readln(l, r, d);
		inc(b[r], d);
		dec(b[l-1], d);
	end;

	for i := n downto 2 do inc(b[i-1], b[i]);

	for i := 1 to n do begin
		write(a[i] + b[i]);
		if i < n then write(' ');
	end;
	writeln;
end.
