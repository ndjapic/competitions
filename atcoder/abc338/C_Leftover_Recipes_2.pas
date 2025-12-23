program C_Leftover_Recipes;
uses
	math;
const
    maxn = 10;
    maxs = 2000 * 1000 + 1;
var
    n, i: int8;
    l, r, s, lx, ly, rx, ry: int32;
    d, dx, dy: int64;
    q, a, b: array [1 .. 26] of int32;

begin
    readln(n);
    for i := 1 to n do read(q[i]); readln;
    for i := 1 to n do read(a[i]); readln;
    for i := 1 to n do read(b[i]); readln;

	l := 0;
	r := maxs;

	while r-l > 1 do begin

		s := (l+r) div 2;
		lx := 0;
		ly := 0;
		rx := s;
		ry := s;

		for i := 1 to n do begin
			if a[i] < b[i] then begin

				if q[i] < int64(s) * a[i] then
					lx := rx+1
				else if q[i] < int64(s) * b[i] then begin
					d := b[i] - a[i];
					dx := int64(b[i]) * s - q[i];
					dy := q[i] - int64(a[i]) * s;
					lx := max(lx, (dx+d-1) div d);
					ry := min(ry, dy div d);
				end;

			end else begin

				if q[i] < int64(s) * b[i] then
					ly := ry+1
				else if q[i] < int64(s) * b[i] then begin
					d := a[i] - b[i];
					dx := q[i] - int64(b[i]) * s;
					dy := int64(a[i]) * s - q[i];
					ly := max(ly, (dy+d-1) div d);
					rx := min(rx, dx div d);
				end;

			end;
		end;

		if (lx <= rx) and (ly <= ry) then
			l := s
		else
			r := s;

	end;

	writeln(l);
end.
