# Problem: D_Pop_and_Insert.pas

```pascal
program D_Pop_and_Insert;
{$MODE DELPHI}
uses
	math;
const
	nn = 500 * 1000;
var
	ntc, tci, n, i, ans0, ans1, l, r: int32;
	s: string;
	c0, c1: array [0 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		readln(s);

		c0[0] := 0;
		c1[0] := 0;
		for i := 1 to n do begin
			c1[i] := c1[i-1] + ord(s[i]) - ord('0');
			c0[i] := i - c1[i];
		end;

		ans0 := high(int32);
		l := 0;
		for r := 1 to n do
			if s[r] = '1' then
				l := r
			else if (r = n) or (s[r+1] = '1') then begin
				ans0 := min(ans0, c0[n] - 2 * (r-l) + n);
				l := r;
			end;

		ans1 := high(int32);
		l := 0;
		for r := 1 to n do
			if s[r] = '0' then
				l := r
			else if (r = n) or (s[r+1] = '0') then begin
				ans1 := min(ans1, c1[n] - 2 * (r-l) + n);
				l := r;
			end;

		writeln(min(ans0, ans1));

	end;
end.

```
