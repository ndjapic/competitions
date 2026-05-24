program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, math;
const
	NN = 200 * 1000;
var
	n, m, j, u, v: int32;
	p, h: array [1 .. NN] of int32;
	child: array [1 .. NN] of tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(u: int32);
var
	v: int32;
begin
	h[u] := 0;
	for v in child[u] do begin
		dfs(v);
		h[u] := max(h[u], h[v]);
	end;
	inc(h[u]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do begin
		read(p[v]);
		child[v] := tlist<int32>.create;
	end;
	readln;

	for j := 1 to m do begin
		readln(u, v);
		if p[u] < p[v] then child[u].add(v);
	end;

	dfs(1);
	writeln(h[1]);
	for v := 1 to n do child[v].free;
end.
