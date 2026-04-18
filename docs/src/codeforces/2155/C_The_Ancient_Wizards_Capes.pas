program C_The_Ancient_Wizards_Capes;
{$MODE DELPHI}
const
	nn = 100 * 1000;
	prime = 676767677;
var
	notc, tci, n, i, ans: int32;
	a: array [1 .. nn] of int32;
	l, r: array [1 .. nn] of boolean;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			read(a[i]);
			l[i] := false;
			r[i] := false;
		end;
		readln;

		for i := 2 to n do
			if a[i-1] < a[i] then begin
				l[i-1] := true;
				l[i] := true;
			end else if a[i-1] > a[i] then begin
				r[i-1] := true;
				r[i] := true;
			end;

		i := 1;
		while (i <= n) and not (l[i] and r[i]) do inc(i);

		if i <= n then
			writeln('0')
		else begin

			ans := 1;
			for i := 1 to n do
				if l[i] = r[i] then begin
					inc(ans, ans);
					if ans >= prime then dec(ans, prime);
				end;

			writeln(ans);

		end;

	end;
end.
