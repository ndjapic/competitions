# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, math;
const
	nn = 100;
var
	n, i, u, v, l, r, x, y: int8;
	m, j: int32;
	play, loop: boolean;
	adj: array [1 .. nn] of tlist<int32>;
	mat: array [1 .. nn, 1 .. nn] of boolean;
	dist, bfs, p: array [1 .. nn] of int8;
	edges: array [1 .. nn*nn div 4] of record
		u, v: int8;
	end;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for v := 1 to n do begin
		adj[v] := tlist<int32>.create;
		dist[v] := n;
		for u := 1 to n do mat[u, v] := false;
	end;

	for i := 1 to n-1 do begin
		readln(u, v);
		adj[u].add(v);
		adj[v].add(u);
		mat[u, v] := true;
		mat[v, u] := true;
	end;

	l := 1;
	r := 1;
	dist[1] := 0;
	bfs[1] := 1;

	while l <= r do begin
		u := bfs[l];
		for v in adj[u] do
			if dist[v] = n then begin
				dist[v] := dist[u] + 1;
				inc(r);
				bfs[r] := v;
			end;
		inc(l);
	end;

	l := 0;
	r := n+1;
	for v := 1 to n do
		if odd(dist[v]) then begin
			dec(r);
			p[r] := v;
		end else begin
			inc(l);
			p[l] := v;
		end;

	m := 0;
	for x := 1 to l do
		for y := r to n do begin
			u := p[x];
			v := p[y];
			if not mat[u, v] then begin
				inc(m);
				edges[m].u := u;
				edges[m].v := v;
			end;
		end;

	play := odd(m);

	if play then
		writeln('First')
	else
		writeln('Second');
	flush(output);

	j := 1;
	loop := true;
	while loop do begin
		if play then begin

			u := edges[j].u;
			v := edges[j].v;
			while (j < m) and mat[u, v] do begin
				inc(j);
				u := edges[j].u;
				v := edges[j].v;
			end;

			if not mat[u, v] then begin
				writeln(min(u, v), ' ', max(u, v));
				flush(output);
			end else
				loop := false;

		end else begin
			readln(u, v);
			loop := u > -1;
		end;

		if loop then begin
			mat[u, v] := true;
			mat[v, u] := true;
			play := not play;
		end;
	end;

	for v := 1 to n do adj[v].free;
end.

```
