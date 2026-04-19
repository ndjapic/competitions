# Problem: B_Unconventional_Pairs.pas

```pascal
program B_Unconventional_Pairs;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, ai: int32;
	ans: int64;
	a: TList<int32>;

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
			ans := 0;

			for i := 0 to n div 2 - 1 do
				ans := max(ans, a[2*i+1] - a[2*i]);

			writeln(ans);

		finally
			a.Free;
		end;

	end;
end.

```
