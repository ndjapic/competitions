# Problem: E_Kite.pas

```pascal
program E_Kite;
{$MODE DELPHI}
(* Longest Increasing Subsequence *)
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;
	inf = int64(1) shl 60;
var
	n, i, a, b, l, r, m, lis: int32;
	x: TList<int64>;
	dp: array [1 .. nn] of int64;

begin
	randomize;
	x := TList<int64>.Create;
	try

		readln(n);

		for i := 0 to n-1 do begin
			readln(a, b);
			x.Add((int64(a+1) shl 30) - b-1);
			x.Exchange(i, random(i+1));
		end;

		x.Sort;

		for i := 0 to n-1 do
			x[i] := -(x[i] and ((1 shl 30) - 1));

		lis := 0;
		for i := 0 to n-1 do begin

			l := 0;
			r := lis+1;
			while r-l > 1 do begin
				m := (l+r) div 2;
				if dp[m] >= x[i] then
					r := m
				else
					l := m;
			end;

			lis := max(lis, r);
			dp[r] := x[i];

		end;

		writeln(lis);

	finally
		x.Free;
	end;
end.

```
