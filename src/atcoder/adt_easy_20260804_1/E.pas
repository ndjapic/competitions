program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #simple #undirected #path #graph
uses
	generics.collections;
const
	NN = 200 * 1000;
var
	n, m, i, u, v, v1, k: int32;
	ans: boolean;
	adj: array [1 .. NN] of tlist<int32>;
	c: array [1 .. 2] of int32;
	seen: array [1 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(u: int32);
var
	v: int32;
begin
	if not seen[u] then begin
		seen[u] := true;
		inc(k);
		for v in adj[u] do dfs(v);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do adj[v] := tlist<int32>.create;

	for i := 1 to m do begin
		readln(u, v);
		adj[u].add(v);
		adj[v].add(u);
	end;

	c[1] := 0;
	c[2] := 0;
	for v := 1 to n do
		if adj[v].count = 1 then begin
			inc(c[1]);
			v1 := v;
		end else if adj[v].count = 2 then
			inc(c[2]);

	ans := (m = n-1) and (c[1] = 2) and (c[1] + c[2] = n);

	if ans then begin

		for v := 1 to n do seen[v] := false;
		k := 0;
		dfs(v1);
		ans := k = n;

	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
