# Problem: C_Isamatdin_and_His_Magic_Wand.pas

```pascal
program C_Isamatdin_and_His_Magic_Wand;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections;
var
	notc, tci, n, i, x: int32;
	a: TList<int32>;
	c: array [0 .. 1] of int32;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		a := TList<int32>.Create;
		try

			readln(n);

			c[0] := 0;
			c[1] := 0;

			for i := 0 to n-1 do begin
				read(x);
				a.Add(x);
				inc(c[x mod 2]);
			end;
			readln;

			if (c[0] > 0) and (c[1] > 0) then begin
				for i := 0 to n-1 do
					a.Exchange(i, random(i+1));
				a.Sort;
			end;

			for i := 0 to n-2 do write(a[i], ' ');
			writeln(a[n-1]);

		finally
			a.Free;
		end;

	end;
end.

```
