# Problem: D_Toggle_Maze.pas

```pascal
program D_Toggle_Maze;
{$MODE DELPHI}
uses
	Generics.Collections, math;
const
	hh = 500;
	inf = 1 shl 30;
type
	TCell = record
		e: int8;
		i, j: int32;
	end;
var
	h, w, i, j, k, d, ans: int32;
	e: int8;
	s, g, u: TCell;
	a: array [0 .. 1] of array [1 .. hh] of string;
	dist: array [0 .. 1] of array [1 .. hh] of array [1 .. hh] of int32;
	bfs: TList<TCell>;

procedure enqueue(e: int8; i, j, d: int32);
var
	v: TCell;
begin
	if (a[e][i][j] <> '#') and (dist[e][i][j] > d) then begin
		dist[e][i][j] := d;
		v.e := e;
		v.i := i;
		v.j := j;
		bfs.Add(v);
	end;
end;

begin
	readln(h, w);

	for i := 1 to h do begin
		readln(a[0][i]);
		setlength(a[1][i], w);

		for j := 1 to w do begin
			a[1][i][j] := a[0][i][j];
			dist[0][i][j] := inf;
			dist[1][i][j] := inf;

			case a[0][i][j] of
				'.': ;
				'#': ;

				'S': begin
					s.i := i;
					s.j := j;
					s.e := 0;
				end;

				'G': begin
					g.i := i;
					g.j := j;
				end;

				'o': a[1][i][j] := '#';
				'x': a[0][i][j] := '#';
				'?': ;
			end;
		end;
	end;

	bfs := TList<TCell>.Create;
	try

		bfs.Add(s);
		dist[0][s.i][s.j] := 0;
		k := 0;

		while k < bfs.Count do begin

			u := bfs[k];
			inc(k);
			i := u.i;
			j := u.j;
			e := u.e;

			d := dist[e][i][j];
			if a[e][i][j] = '?' then
				enqueue(1-e, i, j, d)
			else begin

				inc(d);
				if i > 1 then enqueue(e, i-1, j, d);
				if j > 1 then enqueue(e, i, j-1, d);
				if i < h then enqueue(e, i+1, j, d);
				if j < w then enqueue(e, i, j+1, d);

			end;

		end;

		ans := min(dist[0][g.i][g.j], dist[1][g.i][g.j]);
		if ans = inf then ans := -1;
		writeln(ans);

	finally
		bfs.Free;
	end;
end.

```
