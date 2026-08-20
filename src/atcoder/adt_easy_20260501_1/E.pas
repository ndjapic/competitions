program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 2 * 1000;
	tt = 200 * 1000;
var
	n, t, i, i0, a, r, c, ddf, dsm: int32;
	row, col: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t);

	for i := 1 to n do begin
		row[i] := 0;
		col[i] := 0;
	end;
	ddf := 0;
	dsm := 0;

	i0 := -1;
	for i := 1 to t do begin
		read(a);
		dec(a);

		if i0 = -1 then begin
			r := a div n + 1;
			c := a mod n + 1;
			inc(row[r]);
			inc(col[c]);
			if r = c then inc(ddf);
			if r + c = n + 1 then inc(dsm);
			if (row[r] = n) or (col[c] = n) or (ddf = n) or (dsm = n) then i0 := i;
		end;
	end;
	readln;

	writeln(i0);
end.
