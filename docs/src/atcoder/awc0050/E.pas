program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
const
	nn = 50;
	mm = 18;
	inf = int64(1) shl 60;
var
	n, m, i, j: int8;
	mask: int32;
	eij: int8;
	ans: int64;
	c, e: array [0 .. nn] of int32;
	adj: array [0 .. mm] of tlist<int8>;
	link: array [0 .. 1 shl mm, 0 .. mm] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(j: int8; mask: int32; total: int64);
var
	i: int8;
begin
	if total < ans then begin
		{j := link[mask, j];}
		if j >= m then
			ans := total
		else begin
			if odd(mask shr j) then
				dfs(j+1, mask, total)
			else
				for i in adj[j] do
					dfs(j+1, mask or e[i], total + c[i]);
		end;
	end;
end;

function compare(constref left, right: int8): int32;
begin
	result := + c[left] - c[right];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 0 to n-1 do read(c[i]);
	readln;

	for j := 0 to m-1 do adj[j] := tlist<int8>.create;

	for i := 0 to n-1 do begin
		e[i] := 0;
		for j := 0 to m-1 do begin
			read(eij);
			if eij = 1 then begin
				{if e[i] = 0 then} adj[j].add(i);
				inc(e[i], 1 shl j);
			end;
		end;
		readln;
	end;

	for j := 0 to m-1 do
		adj[j].sort(tcomparer<int8>.construct(compare));

	for mask := 0 to (1 shl m) - 1 do
		for j := m downto 0 do begin
			link[mask, j] := j;
			if odd(mask shr j) then
				link[mask, j] := link[mask, j+1];
		end;

	ans := inf;
	dfs(0, 0, 0);

	if ans = inf then ans := -1;
	writeln(ans);

	for j := 0 to m-1 do adj[j].free;
end.
