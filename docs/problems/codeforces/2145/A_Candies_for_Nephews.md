# Problem: A_Candies_for_Nephews.pas

```pascal
program A_Candies_for_Nephews;
var
	notc, tci, n, ans: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		ans := n mod 3;
		if ans > 0 then ans := 3 - ans;

		writeln(ans);

	end;
end.

```
