# Задатак: A_Permute_to_Maximize.pas

```pascal
program A_Permute_to_Maximize;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections;
var
	i, ai: int8;
	x: int32;
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
		for i := 2 downto 0 do
			x := 10 * x + a[i];

		writeln(x);

	finally
		a.Free;
	end;
end.

```
