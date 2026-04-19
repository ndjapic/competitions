# Problem: C_1122_Substring_2.pas

```pascal
program C_1122_Substring_2;
{$MODE DELPHI}
uses
	math;
var
	n, l1, r1, l2, r2: int32;
	ans: int64;
	s: string;

begin
	readln(s);
	n := length(s);

	l1 := 0;
	l2 := 1;
	ans := 0;
	for r2 := 1 to n do
		if (r2 = n) or (s[r2 + 1] <> s[r2]) then begin

			if (l1 > 0) and (ord(s[r2]) - ord(s[l1]) = 1) then
				inc(ans, min(r2-l2, r1-l1) + 1);

			l1 := l2;
			r1 := r2;
			l2 := r2+1;

		end;

	writeln(ans);
end.

```
