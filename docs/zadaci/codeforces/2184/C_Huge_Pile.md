# Задатак: C_Huge_Pile.pas

```pascal
program C_Huge_Pile;
uses
	math;
var
	notc, tci, n, k, l, r, t: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		l := n;
		r := n;
		t := 0;

		while k < l do begin
			l := l div 2;
			r := (r+1) div 2;
			inc(t);
		end;

		if r < k then t := -1;
		writeln(t);

	end;
end.

```
