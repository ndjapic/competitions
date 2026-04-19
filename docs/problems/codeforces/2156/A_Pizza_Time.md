# Problem: A_Pizza_Time.pas

```pascal
program A_Pizza_Time;
var
	notc, tci, n, ans: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		ans := 0;
		while n > 2 do begin
			inc(ans, n div 3);
			n := n div 3 + n mod 3;
		end;

		writeln(ans);

	end;
end.

```
