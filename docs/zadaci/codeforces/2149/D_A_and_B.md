# Задатак: D_A_and_B.pas

```pascal
program D_A_and_B;
{$MODE DELPHI}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i: int32;
	ansa, ansb: int64;
	s: string;
	a, b: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		a[0] := 0;
		b[0] := 0;
		for i := 1 to n do begin
			a[i] := a[i-1];
			b[i] := b[i-1];
			case s[i] of
				'a': inc(a[i]);
				'b': inc(b[i]);
			end;
		end;

		ansa := 0;
		ansb := 0;
		for i := 1 to n do
			case s[i] of
				'a': inc(ansa, min(b[i], b[n] - b[i]));
				'b': inc(ansb, min(a[i], a[n] - a[i]));
			end;

		writeln(min(ansa, ansb));

	end;
end.

```
