# Задатак: C_Domino.pas

```pascal
program C_Domino;
uses
	math;
var
	n, l, r, h, ans: int32;

begin
	readln(n);
	ans := 0;
	r := 1;

	for l := 1 to n do begin
		read(h);
		if l <= r then begin
			inc(ans);
			r := max(r, l+h-1);
		end;
	end;

	readln;
	writeln(ans);
end.

```
