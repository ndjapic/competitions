program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid #dsu
const
	hh = 1002;
var
	h, w, i, j, c: int32;
	s: array [1 .. hh] of string;
	dsu, size: array [1 .. hh*hh] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ij(i, j: int32): int32;
begin
	result := (i-1) * w + j;
end;

function find(v: int32): int32;
begin
	if dsu[dsu[v]] <> dsu[v] then dsu[v] := find(dsu[v]);
	result := dsu[v];
end;

procedure union(u, v: int32);
begin
	u := find(u);
	v := find(v);
	if u <> v then begin
		if size[u] > size[v] then begin
			dsu[v] := u;
			inc(size[u], size[v]);
		end else begin
			dsu[u] := v;
			inc(size[v], size[u]);
		end;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	inc(h, 2);
	inc(w, 2);

	setlength(s[1], w);
	setlength(s[h], w);
	for j := 1 to w do begin
		s[1][j] := '.';
		s[h][j] := '.';
	end;

	for i := 2 to h-1 do begin
		setlength(s[i], w);
		for j := 2 to w-1 do read(s[i][j]);
		s[i][1] := '.';
		s[i][w] := '.';
		readln;
	end;

	for i := 1 to h do
		for j := 1 to w do begin
			dsu[ij(i, j)] := ij(i, j);
			size[ij(i, j)] := 1;
		end;

	for i := 1 to h do
		for j := 2 to w do
			if (s[i][j-1] = '.') and (s[i][j] = '.') then
				union(ij(i, j-1), ij(i, j));

	for i := 2 to h do
		for j := 1 to w do
			if (s[i-1][j] = '.') and (s[i][j] = '.') then
				union(ij(i-1, j), ij(i, j));

	c := 0;
	for i := 1 to h do
		for j := 1 to w do
			if (s[i][j] = '.') and (find(ij(i, j)) = ij(i, j)) then inc(c);

	writeln(c - 1);
end.
