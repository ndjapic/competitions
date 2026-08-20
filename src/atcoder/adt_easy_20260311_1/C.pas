program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 99;
var
	n, i, j, k, r, c: int32;
	grid: array [0 .. nn, 0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 0 to n-1 do
		for j := 0 to n-1 do
			grid[i, j] := 0;

	i := 0;
	j := (n-1) div 2;
	k := 1;
	grid[i, j] := k;

	while k < n*n do begin
		r := (i-1+n) mod n;
		c := (j+1) mod n;

		if grid[r, c] > 0 then begin
			r := (i+1) mod n;
			c := j;
		end;

		i := r;
		j := c;
		inc(k);
		grid[r, c] := k;
	end;

	for i := 0 to n-1 do begin
		for j := 0 to n-2 do write(grid[i, j], ' ');
		writeln(grid[i, n-1]);
	end;
end.
