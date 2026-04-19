# Problem: B_Hourglass.pas

```pascal
program B_Hourglass;
uses
	math;
var
	notc, tci, s, k, m, d, r: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(s, k, m);

		d := m div k;
		r := m mod k;

		if s < k then begin
			writeln(max(0, s-r))
		end else if odd(d) then
			writeln(k - r)
		else
			writeln(s - r);

	end;
end.

```
