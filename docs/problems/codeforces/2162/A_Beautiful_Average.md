# Problem: A_Beautiful_Average.pas

```pascal
program A_Beautiful_Average;
uses
	math;
var
	notc, tci, n, i, x, ans: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		read(ans);

		for i := 2 to n do begin
			read(x);
			ans := max(ans, x);
		end;
		readln;

		writeln(ans);

	end;
end.

```
