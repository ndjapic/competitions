program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 10;
	INF = 1 shl 30;
var
	n, m, i, a, b: int8;
	c, seen, ans: int32;
	dist: array [1 .. NN, 1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(a: int8; d: int32);
var
	b: int8;
	c: int32;
begin
	if not odd(seen shr a) then begin
		inc(seen, 1 shl a);
		ans := max(ans, d);
		for b := 1 to n do begin
			c := dist[a, b];
			if c < INF then
				dfs(b, d + c);
		end;
		dec(seen, 1 shl a);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do begin
		for b := a+1 to n do begin
			dist[a, b] := INF;
			dist[b, a] := INF;
		end;
		dist[a, a] := 0;
	end;

	for i := 1 to m do begin
		readln(a, b, c);
		dist[a, b] := c;
		dist[b, a] := c;
	end;

	ans := 0;
	seen := 0;

	for a := 1 to n do dfs(a, 0);

	writeln(ans);
end.
