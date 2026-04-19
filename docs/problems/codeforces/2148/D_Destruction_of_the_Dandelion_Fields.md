# Problem: D_Destruction_of_the_Dandelion_Fields.pas

```pascal
program D_Destruction_of_the_Dandelion_Fields;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, x, m: int32;
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

		readln(n);

		ans := 0;
		m := 0;

		for i := 1 to n do begin
			read(x);

			if odd(x) then begin
				inc(m);
				a[m] := x
			end else
				inc(ans, x);
		end;
		readln;

		if m = 0 then
			ans := 0
		else begin
			msort(1, m+1);
			for i := m div 2 + 1 to m do inc(ans, a[i]);
		end;

		writeln(ans);

	end;
end.

```
