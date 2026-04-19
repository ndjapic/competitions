# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
type
	TPath = record
		v, d: int32;
	end;
var
	n, m, k, i, l, r: int32;
	a, b: TPath;
	adj, dis: array [1 .. nn] of int32;
	sib, tar: array [-nn .. nn] of int32;
	bfs: array of TPath;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure addarrow(u, v, i: int32);
begin
	sib[i] := adj[u];
	adj[u] := i;
	tar[i] := v;
end;

procedure readedges(n, m: int32);
var
	u, v, w, i: int32;
begin
	for v := 1 to n do begin
		adj[v] := 0;
		dis[v] := n;
	end;

	for i := 1 to m do begin
		readln(u, v, w);
		if w >= k then begin
			addarrow(u, v, i);
			addarrow(v, u, -i);
		end;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);
	readedges(n, m);

	l := 0;
	r := 1;
	setlength(bfs, 1);
	bfs[0].v := 1;
	bfs[0].d := 0;
	dis[1] := 0;

	while l < r do begin
		a := bfs[l];
		if a.d = dis[a.v] then begin

			b.d := a.d + 1;
			i := adj[a.v];
			while i <> 0 do begin

				b.v := tar[i];
				if dis[b.v] > b.d then begin

					dis[b.v] := b.d;
					if length(bfs) = r then setlength(bfs, 2*r);
					bfs[r] := b;
					inc(r);

				end;
				i := sib[i];

			end;
		end;
		inc(l);
	end;

	if dis[n] = n then dis[n] := -1;
	writeln(dis[n]);
end.

```
