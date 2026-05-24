program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #max #sum
uses
	math;
const
	NN = 100 * 1000;
var
	n, i, j, ans: int32;
	a, w, mx: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	for j := 1 to n do mx[j] := 0;

	ans := 0;
	for i := 1 to n do begin
		j := a[i];
		read(w[i]);
		inc(ans, w[i]);
		mx[j] := max(mx[j], w[i]);
	end;
	readln;

	for j := 1 to n do dec(ans, mx[j]);
	writeln(ans);
end.
