# Problem: A_ Permute_to_Maximize.pas

```pascal
program A_ Permute_to_Maximize;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections;
var
	i, ai: int8;
	a: TList<int8>;

begin
	randomize;
	a := TList<int8>.Create;
	try

		for i := 0 to 2 do begin
			read(ai);
			a.Add(ai);
			a.Exchange(i, random(i+1));
		end;
		readln;
		a.Sort;

		x := 0;
		for ai in a do
			x := 10 * x + ai;

		writeln(x);

	finally
		a.Free;
	end;
end.

```
