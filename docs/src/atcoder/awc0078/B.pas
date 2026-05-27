program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #naive #tle
uses
	math;
const
	NN = 200 * 1000;
	INF = 1 shl 30;
var
	n, i: int32;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(l, r: int32): int64;
var
	i, i0, mn: int32;
begin
	result := 0;
	while (l <= r) and (a[l] = 0) do inc(l);
	while (l <= r) and (a[r] = 0) do dec(r);
	if l <= r then begin
		mn := INF;
		for i := l to r do
			mn := min(mn, a[i]);

		inc(result, mn);

		i0 := l;
		for i := l to r+1 do begin
			if i <= r then dec(a[i], mn);
			if (i > r) or (a[i] = 0) then begin
				inc(result, dfs(i0, i-1));
				i0 := i+1;
			end;
		end;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	writeln(dfs(1, n));
end.
