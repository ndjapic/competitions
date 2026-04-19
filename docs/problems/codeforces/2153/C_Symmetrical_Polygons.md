# Problem: C_Symmetrical_Polygons.pas

```pascal
program C_Symmetrical_Polygons;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000 + 2;
var
	notc, tci, n, m, n1, n2, i, j, ai: int32;
	ans: int64;
	a: TList<int32>;
	s, c, a1, a2: array [1 .. nn] of int32;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		a := TList<int32>.Create;
		try

			for i := 0 to n-1 do begin
				read(ai);
				a.Add(ai);
				a.Exchange(i, random(i+1));
			end;
			readln;
			a.Sort;

			m := 1;
			s[1] := a[0];
			c[1] := 1;

			for i := 1 to n-1 do
				if a[i-1] < a[i] then begin
					inc(m);
					s[m] := a[i];
					c[m] := 1;
				end else
					inc(c[m]);

			n1 := 2;
			n2 := 0;
			a1[1] := 0;
			a1[2] := 0;

			for j := 1 to m do begin
				while c[j] > 1 do begin
					inc(n2);
					a2[n2] := s[j];
					dec(c[j], 2);
				end;
				while c[j] > 0 do begin
					inc(n1);
					a1[n1] := s[j];
					dec(c[j], 1);
				end;
			end;

			ans := 0;
			if n2 > 0 then begin

				for i := 1 to n2 do inc(ans, a2[i]);
				ans := 2 * ans;

				i := n1 - 1;
				while ans + a1[i] <= a1[i+1] do dec(i);
				inc(ans, a1[i] + a1[i+1]);

				if (i = 1) and (n2 = 1) then ans := 0;

			end;
			writeln(ans);

		finally
			a.Free;
		end;

	end;
end.

```
