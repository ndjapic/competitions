# Problem: G.pas

```pascal
program _G; (* WA *)
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections;
const
	nn = 200 * 1000;
var
	n, m, i, u, v, w, l, r: int32;
	moves: int64;
	adj: array [1 .. nn] of tlist<int32>;
	dist, bfs: array [1 .. nn] of int32;
	c: array [0 .. 1] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do begin
		adj[v] := tlist<int32>.create;
		dist[v] := m+1;
	end;

	for i := 1 to m do begin
		readln(u, v);
		adj[u].add(v);
		adj[v].add(u);
	end;

	moves := -1;

	for w := 1 to n do
		if dist[w] > m then begin

			l := 1;
			r := 1;
			dist[w] := 0;
			bfs[1] := w;
			c[0] := 0;
			c[1] := 0;

			while l <= r do begin
				u := bfs[l];
				inc(c[dist[u] mod 2]);
				for v in adj[u] do
					if dist[v] > m then begin
						dist[v] := dist[u] + 1;
						inc(r);
						bfs[r] := v;
					end;
				inc(l);
			end;

			inc(moves, int64(c[0]) * c[1] + 1);

		end;

	if odd(moves - m) then
		writeln('Aoki')
	else
		writeln('Takahashi');

	for v := 1 to n do adj[v].free;
end.

```
