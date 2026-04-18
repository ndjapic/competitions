# Задатак: D_Cheater.pas

```pascal
program D_Cheater;
{$MODE DELPHI}
uses
	math;
const
	nn = 500;
var
    ntc, tci, n, m, i, j, k, li, lj, ri, rj, square, ans: int32;
    mine: array [1 .. nn] of string;
    g: array [0 .. nn, 0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

		for i := 0 to n do g[i, 0] := 0;
		for j := 0 to m do g[0, j] := 0;

		for i := 1 to n do begin
			readln(mine[i]);
			for j := 1 to m do begin
				g[i, j] := g[i, j-1] + g[i-1, j] - g[i-1, j-1];
				if mine[i, j] = 'g' then inc(g[i, j]);
			end;
		end;

		if k > 0 then begin
			ans := -1;
			for i := 1 to n do
				for j := 1 to m do
					if mine[i, j] = '.' then begin
						li := max(0, i-k);
						lj := max(0, j-k);
						ri := min(n, i+k-1);
						rj := min(m, j+k-1);
						square := g[ri, rj] - g[ri, lj] - g[li, rj] + g[li, lj];
						ans := max(ans, g[n, m] - square);
					end;
		end;

		writeln(ans);

    end;
end.

```
