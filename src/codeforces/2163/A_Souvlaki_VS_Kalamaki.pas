program A_Souvlaki_VS_Kalamaki;
const
	nn = 1000;
var
	notc, tci, n, i: int32;
	a, cp: array [1 .. nn] of int32;

procedure MergeSort(lend, rend: int32);
var
	i, l, r, m: int32;
begin
	if rend - lend > 1 then begin
		m := (lend + rend) div 2;
		MergeSort(lend, m);
		MergeSort(m, rend);

		l := lend;
		r := m;
		for i := lend to rend - 1 do
			if (r = rend) or (l < m) and (
				a[l] < a[r]
			) then begin
				cp[i] := a[l];
				inc(l);
			end else begin
				cp[i] := a[r];
				inc(r);
			end;

		for i := lend to rend - 1 do a[i] := cp[i];
	end;
end;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;
		MergeSort(1, n+1);

		i := 3;
		while (i <= n) and (a[i-1] = a[i]) do inc(i, 2);

		if i <= n then
			writeln('NO')
		else
			writeln('YES');

	end;
end.
