# Problem: C_Kanade_s_Perfect_Multiples.pas

```pascal
program C_Kanade_s_Perfect_Multiples;
{$MODE DELPHI}
uses
	Generics.Collections, math;
const
	nn = 100 * 1000;
var
	notc, tci, n, k, m, i, j, x: int32;
	a: TList<int32>;
	b: array [0 .. nn] of int32;
	seen: array [0 .. nn] of boolean;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		a := TList<int32>.Create;

		for i := 0 to n-1 do begin
			read(x);
			a.Add(x);
			a.Exchange(i, random(i+1));
			seen[i] := false;
		end;
		readln;
		a.Sort;

		m := 0;
		i := 0;
		while (m >= 0) and (i < n) do begin
			if not seen[i] then begin

				j := i+1;
				while (j < n) and (a[i] = a[j]) do begin
					seen[j] := true;
					inc(j);
				end;

				x := 2 * a[i];
				while (x <= k) and (m >= 0) do begin
					while (j < n) and (a[j] < x) do inc(j);
					if (j < n) and (a[j] = x) then begin
						seen[j] := true;
						inc(x, a[i]);
					end else
						m := -1;
				end;

				if m >= 0 then begin
					b[m] := a[i];
					inc(m);
				end;

			end;
			inc(i);
		end;

		writeln(m);
		if m >= 0 then begin
			for i := 0 to m-2 do write(b[i], ' ');
			writeln(b[m-1]);
		end;

		a.Free;

	end;
end.

```
