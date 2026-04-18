program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
type
	tcell = record
		i, j: int8;
	end;
var
	h, w, n, i, j, k, h1, h2, c, d, ans: int8;
	s: string;
	floor_cells: array [1 .. nn] of tcell;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dist(k1, k2: int8): int8;
begin
	dist := abs(floor_cells[k1].i - floor_cells[k2].i)
		+ abs(floor_cells[k1].j - floor_cells[k2].j);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, d);

	n := 0;
	for i := 1 to h do begin
		readln(s);
		for j := 1 to w do
			if s[j] = '.' then begin
				inc(n);
				floor_cells[n].i := i;
				floor_cells[n].j := j;
			end;
	end;

	ans := 0;
	for h1 := 1 to n-1 do
		for h2 := h1+1 to n do begin
			c := 0;
			for k := 1 to n do
				if (dist(k, h1) <= d) or (dist(k, h2) <= d) then inc(c);
			ans := max(ans, c);
		end;

	writeln(ans);
end.
