program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #tlist #custom #sort #dsu #components #mst #Kruskal
uses
	Generics.Collections, Generics.Defaults, Math;
const
	nn = 100 * 1000;
type
	tedge = record
		u, v, c: int32;
	end;
var
	n, m, i, c, u, v: int32;
	ans: int64;
	edges: tlist<tedge>;
	edge: tedge;
	dsu, size: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function EdgeCompare(constref Left, Right: tedge): int32;
begin
	Result := CompareValue(Left.c, Right.c);
end;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union2(u, v: int32);
begin
	dsu[v] := u;
	inc(size[u], size[v]);
end;

procedure union1(u, v: int32);
begin
	if size[u] > size[v] then
		union2(u, v)
	else
		union2(v, u);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, m);

	for v := 1 to n do begin
		dsu[v] := v;
		size[v] := 1;
	end;

	edges := tlist<tedge>.Create;
	for i := 0 to m-1 do begin
		readln(edge.u, edge.v, edge.c);
		edges.add(edge);
		edges.exchange(i, random(i+1));
	end;
	edges.Sort(TComparer<tedge>.Construct(EdgeCompare));

	c := n;
	ans := 0;
	for i := 0 to m-1 do begin
		edge := edges[i];
		u := find(edge.u);
		v := find(edge.v);
		if u <> v then begin
			union1(u, v);
			inc(ans, edge.c);
			dec(c);
		end;
	end;

	if c > 1 then ans := -1;
	writeln(ans);
	edges.Free;
end.
