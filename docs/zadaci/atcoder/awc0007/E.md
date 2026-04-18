# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	MM = 15;
	INF = 1 shl 30;
var
	n, mask, maskj: int32;
	m, i, j: int8;
	p: array [0 .. MM+1] of int64;
	r, c: array [0 .. MM+1] of int32;
	dist: array [0 .. MM+1, 0 .. MM+1] of int32;
	dp: array [0 .. 4 shl MM, 0 .. MM+1] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	readln(p[0], p[m+1]);

	for i := 1 to m do read(p[i]);
	if m > 0 then readln;

	for i := 0 to m+1 do begin
		r[i] := (p[i]-1) div n + 1;
		c[i] := (p[i]-1) mod n + 1;
		for j := 0 to i do begin
			dist[i, j] := abs(r[i] - r[j]) + abs(c[i] - c[j]);
			dist[j, i] := dist[i, j];
		end;
		for mask := 0 to 4 shl m do dp[mask, i] := INF;
	end;

	dp[1, 0] := 0;
	for mask := 1 to (4 shl m) - 1 do
		for i := 0 to m+1 do
			if dp[mask, i] < INF then
				for j := 0 to m+1 do
					if mask and (1 shl j) = 0 then begin
						maskj := mask or (1 shl j);
						dp[maskj, j] := min(dp[maskj, j], dp[mask, i] + dist[i, j]);
					end;

	writeln(dp[(1 shl (m+2)) - 1, m+1]);
end.

```
