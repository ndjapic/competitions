program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #pathfinder01 #bfs #queue #set
uses
	Generics.Collections;
const
	NN = 25;
	DD = 7;
	POW2 = 1 shl NN;
var
	n, j, k, d: int8;
	u, v, p0, p2: int32;
	s, t: string;
	adj: TDictionary<int32, boolean>;
	dist: array [0 .. POW2] of int32;
	divs: array [1 .. DD] of int8;
	bfs: TQueue<int32>;
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

	for u := 0 to p0 - 1 do dist[u] := p0;

	readln(s);
	v := s2v(s);
	dist[v] := 0;
	bfs := TQueue<int32>.Create;
	adj := TDictionary<int32, boolean>.Create;
	bfs.Enqueue(v);

	while bfs.Count > 0 do begin
		u := bfs.Dequeue;
		adj.Clear;

		for j := 1 to k do begin
			p2 := 1 shl divs[j];
			v := (p0 - 1) div (p2 - 1) * (u mod p2);
			adj.AddOrSetValue(v, true);
		end;

		v := 2 * u;
		if v >= p0 then dec(v, p0 - 1);
		adj.AddOrSetValue(v, true);

		v := (u mod 2 * p0 + u) div 2;
		adj.AddOrSetValue(v, true);

		d := dist[u] + 1;
		for v in adj.Keys do
			if dist[v] > d then begin
				dist[v] := d;
				bfs.Enqueue(v);
			end;
	end;

	readln(t);
	v := s2v(t);
	if dist[v] = p0 then dist[v] := -1;
	writeln(dist[v]);

	adj.Free;
	bfs.Free;
end.
