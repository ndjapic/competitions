# Problem: B_Cake_Collection.pas

```pascal
program B_Cake_Collection;
uses
	math;
const
	nn = 100 * 1000;
var
	ntc, tci, n, m, i: int32;
	ans: int64;
	a, cp: array [1 .. nn] of int32;

procedure msort(l, r: int32);
var
	m, i, il, ir: int32;
begin
	if r-l > 1 then begin

		m := (l+r) div 2;
		msort(l, m);
		msort(m, r);

		il := l;
		ir := m;
		for i := l to r-1 do
			if (ir >= r) or (il < m) and (a[il] <= a[ir]) then begin
				cp[i] := a[il];
				inc(il);
			end else begin
				cp[i] := a[ir];
				inc(ir);
			end;

		for i := l to r-1 do a[i] := cp[i];

	end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, m);

		for i := 1 to n do read(a[i]); readln; msort(1, n+1);

		ans := 0;
		for i := n downto n+1 - min(n, m) do begin
			inc(ans, int64(m) * a[i]);
			dec(m);
		end;

		writeln(ans);

	end;
end.

```
