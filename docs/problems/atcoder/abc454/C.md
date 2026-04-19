# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	nn = 300 * 1000;
var
	n, m, i, a, b, l, r: int32;
	adj: array [1 .. nn] of tlist<int32>;
	dist, bfs: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do begin
		adj[a] := tlist<int32>.create;
		dist[a] := n;
	end;

	for i := 1 to m do begin
		readln(a, b);
		adj[a].add(b);
	end;

	bfs[1] := 1;
	dist[1] := 0;

	l := 1;
	r := 1;
	while l <= r do begin
		a := bfs[l];
		inc(l);
		for b in adj[a] do
			if dist[b] = n then begin
				dist[b] := dist[a] + 1;
				inc(r);
				bfs[r] := b;
			end;
	end;

	writeln(r);

	for a := 1 to n do adj[a].free;
end.

```
