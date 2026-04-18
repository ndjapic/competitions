# Задатак: B_You_re_a_teapot.pas

```pascal
program B_You_re_a_teapot;
{$MODE DELPHI}
uses
	math;
const
	nn = 100;
var
	n, m, i, j: int8;
	ans: extended;
	s: string;
	a: array [1 .. nn] of int8;

begin
	readln(s);
	n := length(s);

	m := 0;
	ans := 0.0;
	for i := 1 to n do
		if s[i] = 't' then begin
			inc(m);
			a[m] := i;
			for j := 1 to m-1 do
				if a[m] - a[j] >= 2 then
					ans := max(ans, (m-j-1) / (i-a[j]-1));
		end;

	writeln(ans:0:10);
end.

```
