# Problem: B_Tab_Closing.pas

```pascal
program B_Tab_Closing;
var
	notc, tci, a, b, n: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(a, b, n);

		if (a/n < b) and (b < a) then
			writeln(2)
		else
			writeln(1);
	end;
end.

```
