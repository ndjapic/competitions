program E_khba_Loves_to_Sleep;
{$MODE DELPHI}
uses
	math, Generics.Defaults, Generics.Collections;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, j, t, mx: int32;
	k, have, x, l, r, m: int64;
	a: TList<int64>;
	ans: array [0 .. nn] of int32;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k, x);

		a := TList<int64>.Create;
		try

			for i := 0 to n-1 do begin
				read(x);
				a.Add(x);
				a.Exchange(i, random(i+1));
			end;
			readln;
			a.Sort;

			l := 0;
			r := x+1;
			while r-l > 1 do begin
				m := (r+l) div 2;

				if m = 0 then
					have := x+1
				else begin

					have := 0;
					inc(have, max(a[0]-m + 1, 0));
					inc(have, max(x - (a[n-1]+m) + 1, 0));

					for i := 1 to n-1 do
						inc(have, max(a[i] - a[i-1] - 2*m + 1, 0));

				end;

				if have < k then
					r := m
				else
					l := m;
			end;

			if l = 0 then
				for j := 0 to k-1 do ans[j] := j
			else begin

				j := 0;
				for i := 0 to n do begin
					if i = 0 then
						t := 0
					else
						t := a[i-1] + l;

					if i < n then
						mx := a[i] - l
					else
						mx := x;

					while (t <= mx) and (j < k) do begin
						ans[j] := t;
						inc(j);
						inc(t);
					end;
				end;

			end;

			for j := 0 to k-2 do write(ans[j], ' ');
			writeln(ans[k-1]);

		finally
			a.Free;
		end;

	end;
end.
