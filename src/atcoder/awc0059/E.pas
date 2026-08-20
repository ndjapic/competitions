program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #lca #tree #parent #child #sibling #dfs #binary_lifting #tour #ancestor
const
	nn = 300 * 1000;
	ee = 18;
var
	n, q, k, x, y, time: int32;
	e: int8;
	p: array [1 .. nn, 0 .. ee] of int32;
	adj, sib, t1, t2: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(x: int32);
var
	y: int32;
begin
	inc(time);
	t1[x] := time;

	y := adj[x];
	while y > 0 do begin
		dfs(y);
		y := sib[y];
	end;

	inc(time);
	t2[x] := time;
end;

function superior(x, y: int32): boolean;
begin
	result := (t1[x] <= t1[y]) and (t2[y] <= t2[x]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for e := 0 to ee do p[1, e] := 1;

	for x := 1 to n do adj[x] := 0;

	for y := 2 to n do begin
		read(x);
		p[y, 0] := x;
		sib[y] := adj[x];
		adj[x] := y;
		for e := 0 to ee-1 do p[y, e+1] := p[p[y, e], e];
	end;
	readln;

	time := 0;
	dfs(1);

	for k := 1 to q do begin
		readln(x, y);
		if superior(x, y) then
			writeln(x)
		else begin
			for e := ee downto 0 do
				if not superior(p[x, e], y) then x := p[x, e];
			writeln(p[x, 0]);
		end;
	end;
end.
