program C_Leftover_Recipes;
uses
	math;
const
    maxn = 10;
    maxs = 2000 * 1000;
var
    n, i, j: int8;
    s: int32;
    d, dx, dy: int64;
    q, a, b: array [1 .. 26] of int32;

begin
    readln(n);
    for i := 1 to n do read(q[i]); readln;
    for i := 1 to n do read(a[i]); readln;
    for i := 1 to n do read(b[i]); readln;

	s := maxs;
	for i := 1 to n do begin

		{if (a[i] > 0) then s := min(s, q[i] div a[i]);
		if (b[i] > 0) then s := min(s, q[i] div b[i]);}

		for j := i+1 to n do begin
			d := abs(int64(a[i]) * b[j] - int64(a[j]) * b[i]);
			if d <> 0 then begin
				dx := abs(int64(q[i]) * b[j] - int64(q[j]) * b[i]);
				dy := abs(int64(a[i]) * q[j] - int64(a[j]) * q[i]);
				s := min(s, dx div d + dy div d);
			end;
		end;

	end;

	writeln(s);
end.
