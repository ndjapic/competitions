program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #pathfinder #queue #bisect
uses
	math, Generics.Collections;
const
	NN = 100 * 1000;
var
	n, m, i, j, k, u, v, l, r, e: int32;
	h, dist: array [1 .. NN] of int32;
	adj: array [1 .. NN] of tlist<int32>;
	q: TQueue<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for v := 1 to n do begin
		read(h[v]);
		adj[v] := tlist<int32>.create;
	end;
	readln;

	for j := 1 to m do begin
		readln(u, v);
		adj[u].add(v);
		adj[v].add(u);
	end;

	l := 0;
	r := 1 shl 30;

	while r-l > 1 do begin
		e := (l+r) div 2;

		for i := 2 to n do dist[i] := n+1;
		dist[1] := 1;

		if h[1] <= e then begin
			q := TQueue<int32>.Create;
			q.Enqueue(1);

			while q.Count > 0 do begin
				u := q.Dequeue;
				for v in adj[u] do
					if (h[v] <= e) and (dist[v] > dist[u] + 1) then begin
						dist[v] := dist[u] + 1;
						q.Enqueue(v);
					end;
			end;

			q.Free;
		end;

		if dist[n] <= k then
			r := e
		else
			l := e;
	end;

	if r >= 1 shl 30 then r := -1;
	writeln(r);
	for v := 1 to n do adj[v].free;
end.
