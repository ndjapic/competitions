# Problem: C_Flush.pas

```pascal
program C_Flush;
{$MODE DELPHI}
uses
    Generics.Defaults, Generics.Collections, math;
const
	nn = 300 * 1000;
	inf = 1000 * 1000;
var
	n, q, i, j, l, r, m: int32;
	b, x: int64;
	a: TList<int32>;
	s: array [0 .. nn] of int64;

begin
	randomize;
	a := TList<int32>.Create;
	try

		readln(n, q);

		for i := 0 to n-1 do begin
			read(x);
			a.Add(x);
			a.Exchange(i, random(i+1));
		end;
		readln;

		a.Sort;

		s[0] := 0;
		for i := 0 to n-1 do s[i+1] := s[i] + a[i];

		for j := 1 to q do begin
			readln(b);

			if a[n-1] < b then
				x := -1
			else begin

				l := -1;
				r := n;
				while r-l > 1 do begin
					m := (l+r) div 2;
					if a[m] < b then
						l := m
					else
						r := m;
				end;

				x := s[r] + (b-1)*(n-r) + 1;
				if (x > s[n]) then x := -1;

			end;

			writeln(x);
		end;

	finally
		a.Free;
	end;
end.

```
