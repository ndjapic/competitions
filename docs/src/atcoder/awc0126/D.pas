program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #MLE #TLE #pathfinder01 #bfs #queue #set
uses
	Generics.Collections;
const
	NN = 25;
	DD = 7;
	POW2 = 1 shl NN;
type
	TPath = record
		v, d: int32;
	end;
var
	n, j, k, d: int8;
	u, v, p0, p2: int32;
	s, t: string;
	p: TPath;
	adj: array [0 .. POW2] of TDictionary<int32, boolean>;
	dist: array [0 .. POW2] of int32;
	divs: array [1 .. DD] of int8;
	bfs: TQueue<TPath>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function s2v(s: string): int32;
var
	i: int8;
begin
	result := 0;
	for i := n downto 1 do
		result := 2 * result + ord(s[i]) - ord('A');
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	p0 := 1 shl n;
	k := 0;

	for d := 1 to n div 2 do
		if n mod d = 0 then begin
			inc(k);
			divs[k] := d;
		end;

	for u := 0 to p0 - 1 do begin
		adj[u] := TDictionary<int32, boolean>.Create;
		dist[u] := p0;
	end;

	for u := 0 to p0 - 1 do begin

		for j := 1 to k do begin
			p2 := 1 shl divs[j];
			v := (p0 - 1) div (p2 - 1) * (u mod p2);
			if v <> u then
				adj[u].AddOrSetValue(v, true);
		end;

		v := 2 * u;
		if v >= p0 then dec(v, p0 - 1);
		if v <> u then begin
			adj[u].AddOrSetValue(v, true);
			adj[v].AddOrSetValue(u, true);
		end;

	end;

	readln(s);
	p.v := s2v(s);
	p.d := 0;
	dist[p.v] := 0;
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

	readln(t);
	v := s2v(t);
	if dist[v] = p0 then dist[v] := -1;
	writeln(dist[v]);
	for u := 0 to p0 - 1 do adj[u].Free;
	bfs.Free;
end.
