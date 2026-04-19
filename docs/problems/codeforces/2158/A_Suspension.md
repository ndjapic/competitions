# Problem: A_Suspension.pas

```pascal
program A_Suspension;
uses
	math;
var
	notc, tci, n, y, r: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(y, r);
		writeln(min(n, r + y div 2));

	end;
end.

```
