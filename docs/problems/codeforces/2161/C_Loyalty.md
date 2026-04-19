# Problem: C_Loyalty.pas

```pascal
program C_Loyalty;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections;
const
	nn = 100 * 1000;
var
	notc, tci, n, i, x, ai, l, r: int32;
	ans, level: int64;
	a: TList<int32>;
	pre: array [0 .. nn] of int64;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, x);

		a := TList<int32>.Create;
		try

			pre[0] := 0;
			for i := 0 to n-1 do begin
				read(ai);
				pre[i+1] := pre[i] + ai;
				a.Add(ai);
				a.Exchange(i, random(i+1));
			end;
			readln;

			a.Sort;

			l := 0;
			r := n-1;
			ans := 0;

			for i := n downto 1 do begin
				level := pre[i] div x;
				if pre[i] - a[r] < level * x then begin
					inc(ans, a[r]);
					pre[i-1] := pre[i] - a[r];
					dec(r);
				end else begin
					pre[i-1] := pre[i] - a[l];
					inc(l);
				end;
			end;

			writeln(ans);
			for i := 1 to n-1 do write(pre[i] - pre[i-1], ' ');
			writeln(pre[n] - pre[n-1]);

		finally
			a.Free;
		end;

	end;
end.

```
