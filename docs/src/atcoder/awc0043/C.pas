program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	inf = int64(1) shl 60;
var
	n, i, j: int32;
	ans: int64;
	p: array [2 .. nn] of int32;
	w, mx, mn: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 2 to n do read(p[i]);
	readln;

	for i := 1 to n do begin
		read(w[i]);
		mx[i] := -1;
		mn[i] := inf;
	end;
	readln;

	for i := n downto 2 do inc(w[p[i]], w[i]);

	for i := 2 to n do begin
		j := p[i];
		mx[j] := max(mx[j], w[i]);
		mn[j] := min(mn[j], w[i]);
	end;

	ans := 0;
	for i := 1 to n do
		if mx[i] > -1 then
			ans := max(ans, mx[i] - mn[i]);
	writeln(ans);
end.
