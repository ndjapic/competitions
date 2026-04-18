# Задатак: C_Against_the_Difference.pas

```pascal
program C_Against_the_Difference;
{$MODE DELPHI}
uses
	math, Generics.Collections;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, j, x, d: int32;
	ind: array [1 .. nn] of TList<int32>;
	dp: array [0 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for x := 1 to n do ind[x] := TList<int32>.Create;
		try

			dp[0] := 0;
			for i := 1 to n do begin
				dp[i] := dp[i-1];
				read(x);
				ind[x].Add(i);
				d := ind[x].Count - x;

				if d >= 0 then begin
					j := ind[x][d];
					dp[i] := max(dp[i], dp[j-1] + x);
				end;
			end;
			readln;

			writeln(dp[n]);

		finally
			for x := 1 to n do ind[x].Free;
		end;

	end;
end.

```
