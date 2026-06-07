program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dsu #set
uses
	Generics.Collections;
const
	nn = 200 * 1000;
	mm = 150 * 1000;
var
	n, m, j, u, v, c: int32;
	dsu, size, flag: array [1 .. nn] of int32;
	colors: TDictionary<int32, boolean>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union2(u, v, c: int32);
begin
	dsu[v] := u;
	inc(size[u], size[v]);
	flag[u] := c;
end;

procedure union1(u, v, c: int32);
begin
	u := find(u);
	v := find(v);
	if u = v then
		flag[u] := c
	else if size[u] > size[v] then
		union2(u, v, c)
	else
		union2(v, u, c);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do begin
		dsu[v] := v;
		size[v] := 1;
		flag[v] := 0;
	end;

	for j := 1 to m do begin
		readln(u, v, c);
		union1(u, v, c);
	end;

	colors := TDictionary<int32, boolean>.Create;
	for v := 1 to n do
		if (find(v) = v) and (flag[v] > 0) then
			colors.AddOrSetValue(flag[v], true);

	writeln(colors.Keys.Count);
	colors.Free;
end.
