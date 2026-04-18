# Задатак: D_Transmission_Mission.pas

```pascal
program D_Transmission_Mission;
const
	nn = 500 * 1000;
	dd = 1000 * 1000 * 1000 * 1000;
type
	tarr = array of int64;
var
	n, m, i: int32;
	ans: int64;
	x, d, cp: array of int64;

procedure msort(var arr: tarr; l, r: int32);
var
	m, i, il, ir: int32;
begin
	if r-l > 1 then begin

		m := (l+r) div 2;
		msort(arr, l, m);
		msort(arr, m, r);

		il := l;
		ir := m;
		for i := l to r-1 do
			if (ir >= r) or (il < m) and (arr[il] <= arr[ir]) then begin
				cp[i] := arr[il];
				inc(il);
			end else begin
				cp[i] := arr[ir];
				inc(ir);
			end;

		for i := l to r-1 do arr[i] := cp[i];

	end;
end;

begin
	readln(n, m);
	setlength(x, n);
	setlength(cp, n);
	setlength(d, n-1);

	for i := 0 to n-1 do read(x[i]); readln;
	msort(x, 0, n);

	for i := 0 to n-2 do d[i] := x[i+1] - x[i];
	msort(d, 0, n-1);

	ans := 0;
	for i := 0 to n-m-1 do inc(ans, d[i]);
	writeln(ans);
end.

```
