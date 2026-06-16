program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 8;
var
	n, m, i, u, v: int8;
	ans: int32;
	a: array [1 .. NN] of int8;
	seen: array [1 .. NN] of boolean;
	adj: array [1 .. NN, 1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(i: int8; s: int32);
var
	u, v: int8;
begin
	if i < n then begin
		inc(i);
		for v := 1 to n do
			if not seen[v] then begin
				u := 1;
				while (u <= n) and not (adj[u, v] and not seen[u]) do inc(u);

				if u > n then begin
					seen[v] := true;
					dfs(i, s + a[v] * i);
					seen[v] := false;
				end;
			end;
	end else
		ans := max(ans, s);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do begin
		read(a[v]);
		seen[v] := false;
		for u := 1 to n do adj[u, v] := false;
	end;
	readln;

	for i := 1 to m do begin
		readln(u, v);
		adj[u, v] := true;
	end;

	ans := 0;
	dfs(0, 0);
	writeln(ans);
end.
