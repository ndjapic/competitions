# Problem: A_Sublime_Sequence.pas

```pascal
program A_Sublime_Sequence;
var
	ntc, tci, n, x: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(x, n);
		writeln(n mod 2 * x);

	end;
end.

```
