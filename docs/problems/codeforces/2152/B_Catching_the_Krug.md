# Problem: B_Catching_the_Krug.pas

```pascal
program B_Catching_the_Krug;
uses
	math;
var
	notc, tci, n, rk, ck, rd, cd, ans: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, rk, ck, rd, cd);

		if rk > rd then begin

			if ck > cd then
				ans := max(n-rd, n-cd)
			else if ck < cd then
				ans := max(n-rd, cd)
			else
				ans := n-rd;

		end else if rk < rd then begin

			if ck > cd then
				ans := max(rd, n-cd)
			else if ck < cd then
				ans := max(rd, cd)
			else
				ans := rd;

		end else begin

			if ck > cd then
				ans := n-cd
			else if ck < cd then
				ans := cd
			else
				ans := 1;

		end;

		writeln(ans);

	end;
end.

```
