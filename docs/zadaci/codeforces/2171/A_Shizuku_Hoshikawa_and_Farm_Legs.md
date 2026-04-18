# Задатак: A_Shizuku_Hoshikawa_and_Farm_Legs.pas

```pascal
program A_Shizuku_Hoshikawa_and_Farm_Legs;
var
	notc, tci, n, ans: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		if odd(n) then
			ans := 0
		else
			ans := n div 4 + 1;

		writeln(ans);

	end;
end.

```
