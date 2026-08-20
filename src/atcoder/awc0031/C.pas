program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1500;
var
	n, d, s, t, i, j, l, r: int32;
	dd: int64;
	x, y: array [1 .. nn] of int32;
	dist: array [1 .. nn] of int32;
	bfs: array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function d2(i, j: int32): int64;
var
	dx, dy: int32;
begin
	dx := x[i] - x[j];
	dy := y[i] - y[j];
	d2 := sqr(int64(dx)) + sqr(int64(dy));
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d, s, t);
	dd := d*d;

	for i := 1 to n do readln(x[i], y[i]);

	l := 0;
	r := 1;
	setlength(bfs, 1);
	bfs[0] := s;
	for i := 1 to n do dist[i] := n;
	dist[s] := 0;

	while l < r do begin
		i := bfs[l];
		inc(l);

		for j := 1 to n do
			if (d2(i, j) <= dd) and (dist[j] > dist[i] + 1) then begin
				dist[j] := dist[i] + 1;
				if length(bfs) = r then setlength(bfs, 2*r);
				bfs[r] := j;
				inc(r);
			end;
	end;

	if dist[t] = n then
		writeln(-1)
	else
		writeln(dist[t]);
end.
