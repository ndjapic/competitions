# Problem: C_Leftover_Recipes.pas

```pascal
program C_Leftover_Recipes;
uses
	math;
const
    maxn = 10;
    maxs = 2000 * 1000 + 1;
var
    n, i: int8;
    l, r, lx, ly, rx, ry: int32;
    s: int64;
    q, a, b: array [1 .. 26] of int32;

function f(i: int8; x, y: int64): int64;
begin
    f := x * a[i] + y * b[i];
end;

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
		rx := s;
		ly := s;
		ry := 0;

        i := 1;
        while (i <= n) and (lx <= rx) do begin

            if min(f(i, lx, ly), f(i, rx, ry)) > q[i] then
                lx := rx+1
			else if f(i, lx, ly) > q[i] then begin

                (* ax+by=q *)
                (* x+y=s *)
                (* a(s-y)+by=q *)
                (* (b-a)y=q-as *)
                (* y=(q-as)/(b-a) *)

                ly := min(ly, (q[i] - s * a[i]) div (b[i] - a[i]));
                lx := s - ly;

			end else if f(i, rx, ry) > q[i] then begin

                (* ax+by=q *)
                (* x+y=s *)
                (* ax+b(s-x)=q *)
                (* (a-b)x=q-bs *)
                (* x=(q-bs)/(a-b) *)

                rx := min(rx, (q[i] - s * b[i]) div (a[i] - b[i]));
                ry := s - rx;

			end;

            inc(i);
		end;

		if lx <= rx then
			l := s
		else
			r := s;

	end;

	writeln(l);
end.

```
