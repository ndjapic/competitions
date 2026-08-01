program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #graph #dfs #isomorphism
uses
	math;
const
	NN = 8;
var
	n, i, mg, mh, u, v: int8;
	ans: int32;
	g, h: array [1 .. NN, 1 .. NN] of boolean;
	a: array [1 .. NN, 1 .. NN] of int32;
	p: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(u: int8; seen: int16);
var
	v: int8;
	cost: int32;
begin
	if u < n then begin
		inc(u);
		for v := 1 to n do
			if not odd(seen shr v) then begin
				p[u] := v;
				dfs(u, seen or (1 shl v));
			end;
	end else begin
		cost := 0;
		for u := 1 to n-1 do
			for v := u+1 to n do
				if g[p[u], p[v]] <> h[u, v] then inc(cost, a[u, v]);
		ans := min(ans, cost);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for u := 1 to n do
		for v := 1 to n do begin
			g[u, v] := false;
			h[u, v] := false;
		end;

	readln(mg);
	for i := 1 to mg do begin
		readln(u, v);
		g[u, v] := true;
		g[v, u] := true;
	end;

	readln(mh);
	for i := 1 to mh do begin
		readln(u, v);
		h[u, v] := true;
		h[v, u] := true;
	end;

	for u := 1 to n-1 do begin
		for v := u+1 to n do read(a[u][v]);
		readln;
	end;

	ans := 1 shl 30;
	dfs(0, 0);
	writeln(ans);
end.
