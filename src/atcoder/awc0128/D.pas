program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 500 * 1000;
var
	n, m, t, i, j, l, r, d, ans: int32;
	f: array [1 .. NN] of int64;
	df: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, t);

	for i := 1 to n do begin
		read(f[i]);
		df[i] := 0;
	end;
	readln;

	for j := 1 to m do begin
		readln(l, r, d);
		inc(df[r], d);
		dec(df[l-1], d);
	end;

	ans := 0;
	for i := n downto 1 do begin
		if f[i] - df[i] <= t then inc(ans);
		inc(df[i-1], df[i]);
	end;

	writeln(ans);
end.
