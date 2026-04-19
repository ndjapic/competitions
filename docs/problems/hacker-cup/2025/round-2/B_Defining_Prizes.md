# Problem: B_Defining_Prizes.pas

```pascal
program B_Defining_Prizes;
uses
	math;
const
	nn = 1000 * 1000 + 1;
type
	tarr = array of int32;
var
	notc, tci, n, m, i, j, k, l, r, x, t, mn: int32;
	{s: int64;
	ans: boolean;}
	a, b, cp, ind: tarr;

procedure sort(var arr: tarr; lend, rend: sizeint);
var
	i, l, r, m: sizeint;

begin
	if rend - lend > 1 then begin
		m := (lend + rend) div 2;
		sort(arr, lend, m);
		sort(arr, m, rend);

		l := lend;
		r := m;
		for i := lend to rend - 1 do
			if (r = rend) or (l < m) and (arr[l] <= arr[r]) then begin
				cp[i] := arr[l];
				inc(l);
			end else begin
				cp[i] := arr[r];
				inc(r);
			end;

		for i := lend to rend - 1 do arr[i] := cp[i];
	end;
end;

begin
	setlength(a, nn);
	setlength(b, nn);
	setlength(cp, nn);
	setlength(ind, nn);

	readln(notc);
	for tci := 1 to notc do begin
		readln(n, m);
		for i := 0 to n-1 do read(a[i]); readln; sort(a, 0, n);
		for j := 0 to m-1 do read(b[j]); readln; sort(b, 0, m);

		t := 0;
		ind[0] := 0;

		for i := 1 to n-1 do
			if a[i-1] < a[i] then begin
				inc(t);
				ind[t] := i;
			end;

		inc(t);
		ind[t] := n;

		l := -1;
		r := nn+1;
		while r-l > 1 do begin
			x := (l+r) div 2;

			k := 0;
			while (k < t) and (a[ind[k]] < x) do inc(k);

			j := m-1;
			i := n;
			{ans := true;}

			{s := 0;}
			for j := m-1 downto 0 do
				if k < t then begin
					mn := min(b[j], n - ind[k]);
					dec(i, mn);
					{inc(s, mn);}
					if (k < t) and (i <= ind[k]) then begin
						inc(i, n - ind[k]);
						inc(k);
					end;
				end {else
					ans := false};

			if (t < k) or (i < n) then
				l := x
			else
				r := x;
		end;

		k := 0;
		while (k < t) and (a[ind[k]] < r) do inc(k);

		writeln('Case #', tci, ': ', n - ind[k]);
	end;
end.

```
