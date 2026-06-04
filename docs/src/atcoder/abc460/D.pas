program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 1000 * 1000;
type
	TCell = record
		i, j: int32;
	end;
var
	h, w, i, j, l, r: int32;
	s: string;
	d: array [1 .. HH] of array of int32;
	u, v: tcell;
	bfs: array [1 .. HH] of tcell;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	l := 1;
	r := 0;

	for i := 1 to h do begin
		setlength(d[i], w+1);
		readln(s);
		for j := 1 to w do
			case s[j] of

				'#' : begin
					d[i][j] := 0;
					inc(r);
					bfs[r].i := i;
					bfs[r].j := j;
				end;

				'.': d[i][j] := HH;

			end;
	end;

	while l <= r do begin
		u := bfs[l];
		inc(l);

		for i := u.i-1 to u.i+1 do
			for j := u.j-1 to u.j+1 do
				if (1 <= i) and (i <= h) and (1 <= j) and (j <= w) and (d[i][j] >= HH) then
					d[i][j] := 1;
	end;

	l := 1;
	r := 0;

	for i := 1 to h do
		for j := 1 to w do
			if d[i][j] = 1 then begin
				inc(r);
				bfs[r].i := i;
				bfs[r].j := j;
			end else {if d[i][j] > 0 then}
				d[i][j] := HH;

	if l <= r then begin
		while l <= r do begin
			u := bfs[l];
			inc(l);

			for i := u.i-1 to u.i+1 do
				for j := u.j-1 to u.j+1 do
					if (1 <= i) and (i <= h) and (1 <= j) and (j <= w) and (d[i][j] >= HH) then begin
						d[i][j] := d[u.i][u.j] + 1;
						v.i := i;
						v.j := j;
						inc(r);
						bfs[r] := v;
					end;
		end;

		for i := 1 to h do begin
			for j := 1 to w do
				if odd(d[i][j]) then
					s[j] := '.'
				else
					s[j] := '#';
			writeln(s);
		end;
	end else
		for i := 1 to h do begin
			for j := 1 to w do s[j] := '.';
			writeln(s);
		end;

end.
