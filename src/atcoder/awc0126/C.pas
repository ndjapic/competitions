program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #graph #pathfinder01 #set #queue
uses
	Generics.Collections;
const
	NN = 200 * 1000;
type
	TPath = record
		v, d: int32;
	end;
var
	n, m, i, u, v: int32;
	s: int8;
	p: TPath;
	adj: array [1 .. NN] of TDictionary<int32, boolean>;
	dist: array [1 .. NN] of int32;
	bfs: TQueue<TPath>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do begin
		adj[v] := TDictionary<int32, boolean>.Create;
		dist[v] := n;
	end;
	dist[1] := 0;

	for i := 1 to m do begin
		readln(u, v, s);
		if s = 1 then begin
			adj[u].AddOrSetValue(v, true);
			adj[v].AddOrSetValue(u, true);
		end;
	end;

	p.v := 1;
	p.d := 0;
	bfs := TQueue<TPath>.Create;
	bfs.Enqueue(p);

	while bfs.Count > 0 do begin
		p := bfs.Dequeue;
		u := p.v;

		if dist[u] = p.d then begin
			inc(p.d);
			for v in adj[u].Keys do
				if dist[v] > p.d then begin
					dist[v] := p.d;
					p.v := v;
					bfs.Enqueue(p);
				end;
		end;
	end;

	for v := 1 to n do adj[v].Free;
	bfs.Free;

	if dist[n] = n then dist[n] := -1;
	writeln(dist[n]);
end.
