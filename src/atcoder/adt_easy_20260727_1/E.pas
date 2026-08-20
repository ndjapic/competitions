program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 200 * 1000;
var
	n, k, i, j, x, h: int32;
	ans: int64;
	a: array [0 .. NN] of int32;
	b: array [1 .. 2 * NN] of int32;
	pre, suf: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	a[0] := 0;
	a[k+1] := n+1;
	x := 0;

	for j := 1 to k+1 do begin
		if j <= k then read(a[j]);

		for i := a[j-1] + 1 to a[j] - 1 do begin
			inc(x);
			b[x] := i;
			inc(x);
			b[x] := i;
		end;

		if j <= k then begin
			inc(x);
			b[x] := a[j];
		end;
	end;
	readln;

	h := (2 * n - k) div 2;
	pre[0] := 0;
	suf[0] := 0;

	for i := 1 to h do begin
		pre[i] := pre[i-1] + abs(b[2*i-1] - b[2*i]);
		suf[i] := suf[i-1] + abs(b[2*(h-i+1)+1] - b[2*(h-i+1)]);
	end;

	ans := high(int64);
	for i := 0 to h do ans := min(ans, pre[i] + suf[h-i]);
	writeln(ans);
end.
