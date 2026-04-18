# Задатак: C_Blackslex_and_Number_Theory.pas

```pascal
program C_Blackslex_and_Number_Theory;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
var
	notc, tci, n, i, x: int32;
	a: TList<int32>;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		a := TList<int32>.Create;

		for i := 0 to n-1 do begin
			read(x);
			a.Add(x);
			a.Exchange(i, random(i+1));
		end;
		readln;
		a.Sort;

		writeln(max(a[0], a[1] - a[0]));

		a.Free;

	end;
end.

```
