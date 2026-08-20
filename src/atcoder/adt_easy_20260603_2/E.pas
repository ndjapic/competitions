program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dsu #default #sort
uses
	generics.collections,
	generics.defaults, math;
const
	NN = 200 * 1000;
var
	n, i, v, u, ans: int32;
	a, b: array [1 .. NN] of int32;
	dsu, size: array [0 .. 2*NN] of int32;
	h: tlist<int32>;
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

function bisect(x: int32): int32;
var
	l, r, m: int32;
begin
	l := 0;
	r := 2*n+1;
	while r-l > 1 do begin
		m := (l+r) div 2;
		if h[m] > x then
			r := m
		else
			l := m;
	end;
	result := l;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	for v := 0 to 2*n do begin
		dsu[v] := v;
		size[v] := 1;
	end;

	h := tlist<int32>.create;
	h.add(1);

	for i := 1 to n do begin
		readln(a[i], b[i]);
		h.add(a[i]);
		h.add(b[i]);
	end;

	for v := 1 to 2*n do h.exchange(v, random(v+1));
	h.sort;

	for i := 1 to n do union1(bisect(a[i]), bisect(b[i]));

	ans := 1;
	u := find(bisect(1));
	for v := 1 to 2*n do
		if find(v) = u then
			ans := max(ans, h[v]);

	writeln(ans);
	h.free;
end.
