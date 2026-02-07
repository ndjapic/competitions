program D_Tail_of_Snake;
uses
	math;
const
	nn = 300 * 1000;
var
	n, i, j: int32;
	a: array [1 .. 3, 1 .. nn] of int64;

begin
	readln(n);

	for i := 1 to 3 do begin
		for j := 1 to n do read(a[i, j]); readln;
	end;

	for j := 2 to n do inc(a[1, j], a[1, j-1]);

	for i := 2 to 3 do begin
		inc(a[i, i], a[i-1, i-1]);
		for j := i+1 to n do
			inc(a[i, j], max(a[i-1, j-1], a[i, j-1]));
	end;

	writeln(a[3, n]);
end.
