program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
const
	NN = 200 * 1000;
var
	n, i, u, v, x, y, z: int32;
	par, d: array [1 .. NN] of int32;
	adj: array [1 .. NN] of TList<int32>;
	l, r, a: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(u: int32);
var
	v: int32;
begin
	for v in adj[u] do
		if par[u] <> v then begin
			par[v] := u;
			d[v] := d[u] + 1;
			dfs(v);
		end;
end;

procedure lift(var x: int32);
begin
	
end;

function lca(x, y: int32): int32;
begin
	while d[x] > d[y] do begin
		l.Add(x);
		x := par[x];
	end;

	while d[y] > d[x] do begin
		r.Add(y);
		y := par[y];
	end;

	while x <> y do begin
		l.Add(x);
		r.Add(y);
		x := par[x];
		y := par[y];
	end;

	Result := x;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x, y);

	for v := 1 to n do
		adj[v] := TList<int32>.Create;

	for i := 1 to n-1 do begin
		readln(u, v);
		adj[u].Add(v);
		adj[v].Add(u);
	end;

	par[1] := 1;
	d[1] := 0;
	dfs(1);

	l := TList<int32>.Create;
	r := TList<int32>.Create;
	a := TList<int32>.Create;
	z := lca(x, y);

	for i := 0 to l.Count - 1 do a.Add(l[i]);
	a.Add(z);
	for i := r.Count - 1 downto 0 do a.Add(r[i]);

	for i := 0 to a.Count - 2 do write(a[i], ' ');
	writeln(a[a.Count - 1]);

	for v := 1 to n do
		adj[v].Free;
	l.Free;
	r.Free;
	a.Free;
end.
