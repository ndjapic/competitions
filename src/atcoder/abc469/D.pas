program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #graph #custom #sort #unsolved #WA
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 200 * 1000;
var
	n, m, i, a, b, d, o: int32;
	ans: int64;
	adj: array [1 .. NN] of TList<int32>;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function PlayerCompare(constref l, r: int32): int32;
begin
	Result := CompareValue(adj[l].Count, adj[r].Count);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, m);

	for a := 1 to n do adj[a] := TList<int32>.Create;

	for i := 1 to m do begin
		readln(a, b);
		adj[a].Add(b);
		adj[b].Add(a);
	end;

	p := TList<int32>.Create;
	for a := 1 to n do begin
		p.Add(a);
		p.Exchange(a-1, Random(a));
	end;
	p.Sort(TComparer<int32>.Construct(PlayerCompare));

	ans := 0;
	o := n;
	for a in p do begin
		d := m - adj[a].Count;
		while (o > 0) and (adj[p[o-1]].Count >= d) do dec(o);
		inc(ans, n-o);
		if adj[a].Count >= d then dec(ans);
		for b in adj[a] do
			if adj[b].Count = d then dec(ans);
	end;

	writeln(ans div 2);
	for a := 1 to n do adj[a].Free;
end.
