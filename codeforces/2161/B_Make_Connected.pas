program B_Make_Connected;
{$MODE DELPHI}
const
	nn = 100;
	nn2 = 100 * 100;
var
	notc, tci, n, i, j, comp, l, r: int32;
	s: array [1 .. nn] of string;
	dsu, size, bfs: array [1 .. nn2] of int32;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	find := dsu[v];
end;

procedure union(u, v: int32);
begin
	u := find(u);
	v := find(v);
	if u <> v then begin
		dec(comp);
		if size[u] > size[v] then begin
			dsu[v] := u;
			inc(size[u], size[v]);
		end else begin
			dsu[u] := v;
			inc(size[v], size[u]);
		end;
	end;
end;

function ij(i, j: int32): int32;
begin
	ij := (i-1) * n + j;
end;

procedure dfs(i, j: int32);
var
	w: int8;
begin
	if s[i][j] = '.' then begin
		w := 0;
		if (i > 1) and (s[i-1][j] = '.') then inc(w);
		if (i < n) and (s[i+1][j] = '.') then inc(w);
		if (j > 1) and (s[i][j-1] = '.') then inc(w);
		if (j < n) and (s[i][j+1] = '.') then inc(w);
	end;
end;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		comp := 0;
		for i := 1 to n do begin
			readln(s[i]);
			for j := 1 to n do begin
				if s[i][j] = '#' then inc(comp);
		end;

		for i := 2 to n do
			for j := 1 to n do
				if (s[i][j] = '#') and (s[i-1][j] = '#') then
					union(ij(i, j), ij(i-1, j));

		for i := 1 to n do
			for j := 2 to n do
				if (s[i][j] = '#') and (s[i][j-1] = '#') then
					union(ij(i, j), ij(i, j-1));

		for i := 1 to n do
			for j := 1 to n do
				if s[i][j] = '#' then begin
					if (i < n) and (s[i+1][j] = '.') then
				end;

	end;
end.
