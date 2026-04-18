# Задатак: D_Teleport_Maze.pas

```pascal
program D_Teleport_Maze;
{$INLINE ON}
const
	size = 1000;
	size2 = sqr(size);
type
	TCell = record
		i, j: int32;
	end;
var
	h, w, i, j, k, l, r, d: int32;
	ch: char;
	s: array [1 .. size] of string;
	dist: array [1 .. size, 1 .. size] of int32;
	t: array ['a' .. 'z'] of int32;
	seen: array ['a' .. 'z'] of boolean;
	warps: array ['a' .. 'z', 1 .. size2] of TCell;
	q: array of TCell;

procedure enqueue(i, j, d: int32); inline;
begin
	if (s[i][j] <> '#') and (dist[i][j] > d) then begin
		dist[i][j] := d;
		if length(q) = r then setlength(q, 2*r+1);
		q[r].i := i;
		q[r].j := j;
		inc(r);
	end;
end;

begin
	readln(h, w);

	for ch := 'a' to 'z' do begin
		t[ch] := 0;
		seen[ch] := false;
	end;

	for i := 1 to h do begin
		readln(s[i]);
		for j := 1 to w do begin
			ch := s[i][j];
			if ('a' <= ch) and (ch <= 'z') then begin
				inc(t[ch]);
				warps[ch][t[ch]].i := i;
				warps[ch][t[ch]].j := j;
			end;
		end;
	end;

	for i := 1 to h do
		for j := 1 to w do dist[i, j] := size2;

	l := 0;
	r := 0;
	setlength(q, 0);
	enqueue(1, 1, 0);

	while l < r do begin
		i := q[l].i;
		j := q[l].j;
		ch := s[i][j];
		inc(l);
		d := dist[i][j] + 1;

		if j < w then enqueue(i, j+1, d);
		if j > 1 then enqueue(i, j-1, d);
		if i < h then enqueue(i+1, j, d);
		if i > 1 then enqueue(i-1, j, d);

		if (ch <> '.') and not seen[ch] then begin
			for k := 1 to t[ch] do
				enqueue(warps[ch, k].i, warps[ch, k].j, d);
			seen[ch] := true;
		end;
	end;

	if dist[h][w] = size2 then
		writeln(-1)
	else
		writeln(dist[h][w]);
end.

```
