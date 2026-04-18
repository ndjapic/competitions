# Задатак: D_Sum_of_Differences.pas

```pascal
program D_Sum_of_Differences;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 300 * 1000;
	prime = 998244353;
var
	n, m, i, j: int32;
	x, ans: int64;
	a, b: TList<int64>;
	s: array [0 .. nn] of int64;

begin
	randomize;
	a := TList<int64>.Create;
	b := TList<int64>.Create;
	try

		readln(n, m);

		for i := 0 to n-1 do begin
			read(x);
			a.Add(x);
			a.Exchange(i, random(i+1));
		end;
		readln;
		a.Sort;

		for j := 0 to m-1 do begin
			read(x);
			b.Add(x);
			b.Exchange(j, random(j+1));
		end;
		readln;
		b.Sort;

		s[0] := 0;
		for j := 1 to m do begin
			s[j] := s[j-1] + b[j-1];
			if s[j] >= prime then dec(s[j], prime);
		end;

		j := 0;
		ans := 0;
		for i := 0 to n-1 do begin
			while (j < m) and (a[i] > b[j]) do inc(j);
			inc(ans, a[i] * j + (prime - a[i]) * (m-j));
			inc(ans, s[m] + 2 * (prime - s[j]));
			ans := ans mod prime;
		end;

		writeln(ans);

	finally
		a.Free;
		b.Free;
	end;
end.

```
