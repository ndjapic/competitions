program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 10;
var
	n, i, j, k, x, y, dx, dy: int32;
	num, ans: int64;
	a: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function digit(i, j: int32): int8;
var
	x, y: int32;
begin
	x := (i-1) mod n + 1;
	y := (j-1) mod n + 1;
	result := ord(a[x][y]) - ord('0');
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(a[i]);

	ans := 0;
	for i := 1 to n do
		for j := 1 to n do
			for dx := -1 to 1 do
				for dy := -1 to 1 do
					if abs(dx) + abs(dy) > 0 then begin
						num := 0;
						x := i + n;
						y := j + n;

						for k := 0 to n-1 do begin
							num := 10 * num + digit(x, y);
							inc(x, dx);
							inc(y, dy);
						end;

						ans := max(ans, num);
					end;

	writeln(ans);
end.
