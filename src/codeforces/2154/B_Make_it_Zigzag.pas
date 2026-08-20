program B_Make_it_Zigzag;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, mx, d: int32;
	ans: int64;
	a: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		mx := 0;
		ans := 0;
		for i := 1 to n do begin
			mx := max(mx, a[i]);
			if not odd(i) then begin

				a[i] := mx;
				d := max(a[i-1] - a[i] + 1, 0);
				dec(a[i-1], d);
				inc(ans, d);

			end else if i > 1 then begin

				d := max(a[i] - a[i-1] + 1, 0);
				dec(a[i], d);
				inc(ans, d);

			end;
		end;

		writeln(ans);

	end;
end.
