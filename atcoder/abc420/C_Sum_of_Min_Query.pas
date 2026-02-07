program C_Sum_of_Min_Query;
uses
	math;
const
	nn = 200 * 1000;
var
	n, k, q, i, x, v: int32;
	s: int64;
	c: char;
	a, b: array [1 .. nn] of int32;

begin
	readln(n, q);

	for k := 1 to n do read(a[k]); readln;
	for k := 1 to n do read(b[k]); readln;

	s := 0;
	for k := 1 to n do inc(s, min(a[k], b[k]));

	for i := 1 to q do begin
		readln(c, x, v);
		dec(s, min(a[x], b[x]));
		case c of
			'A': a[x] := v;
			'B': b[x] := v;
		end;
		inc(s, min(a[x], b[x]));
		writeln(s);
	end;
end.
