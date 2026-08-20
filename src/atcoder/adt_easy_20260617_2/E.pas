program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000 * 1000;
var
	n, q, i, j, x, y, mn, ans: int32;
	c: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do c[i] := 1;

	mn := 1;
	for j := 1 to q do begin
		readln(x, y);
		ans := 0;

		while mn <= x do begin
			inc(ans, c[mn]);
			c[mn] := 0;
			inc(mn);
		end;

		inc(c[y], ans);
		writeln(ans);
	end;
end.
