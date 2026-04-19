# Problem: C_Alternated.pas

```pascal
program C_Alternated;
{$MODE DELPHI}
uses
	math;
const
	nn = 500 * 1000;
var
	n, i, j: int32;
	ans1, ans2: int64;
	s: string;
	a: array [1 .. nn] of int32;

begin
	readln(n);
	readln(s);

	j := 0;
	for i := 1 to 2*n do
		if s[i] = 'A' then begin
			inc(j);
			a[j] := i;
		end;

	ans1 := 0;
	ans2 := 0;
	for j := 1 to n do begin
		inc(ans1, abs(2*j-1 - a[j]));
		inc(ans2, abs(2*j-0 - a[j]));
	end;

	writeln(min(ans1, ans2));
end.

```
