# Задатак: A_Collatz_Conjecture.pas

```pascal
program A_Collatz_Conjecture;
{$MODE DELPHI}
uses
	math;
const
	nn = 100;
var
	ntc, tci, k, x: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(k, x);
		writeln(x shl k);

	end;
end.

```
