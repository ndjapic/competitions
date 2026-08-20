program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, m, k, i, u, v: int32;
	dsu, size: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

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
	u := find(u);
	v := find(v);
	if u = v then
	else if size[u] > size[v] then
		union2(u, v)
	else
		union2(v, u);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for v := 0 to n do begin
		dsu[v] := v;
		size[v] := 1;
	end;

	for i := 1 to k do begin
		read(v);
		union1(0, v);
	end;
	readln;

	for i := 1 to m do begin
		readln(u, v);
		union1(u, v);
	end;

	writeln(size[find(0)] - 1);
end.
