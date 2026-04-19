# Problem: C_Monocarp_s_String.pas

```pascal
program C_Monocarp_s_String;
{$MODE DELPHI}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, ans: int32;
	s: string;
	c: array [0 .. nn] of int32;
	ind: array [-nn .. nn] of int32;

begin
	c[0] := 0;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		for i := 1 to n do
			case s[i] of
				'a': c[i] := c[i-1] + 1;
				'b': c[i] := c[i-1] - 1;
			end;

		for i := -n to n do ind[i] := -n;

		ans := n;
		for i := 0 to n do begin
			ind[c[i]] := i;
			ans := min(ans, i - ind[c[i] - c[n]]);
		end;

		if ans = n then ans := -1;
		writeln(ans);

	end;
end.

```
