# Problem: B_Most_Minority.pas

```pascal
program B_Most_Minority;
{$MODE DELPHI}
uses
	math;
const
	nn = 100;
var
	n, m, i, j, k, x, y, mx: int8;
	s: array [1 .. nn] of string;
	score, ans: array [1 .. nn] of int8;

begin
	readln(n, m);

	for i := 1 to n do begin
		readln(s[i]);
		score[i] := 0;
	end;

	for j := 1 to m do begin
		y := 0;
		for i := 1 to n do
			inc(y, ord(s[i][j]) - ord('0'));
		x := n-y;

		if (x = 0) or (y = 0) then begin
			for i := 1 to n do inc(score[i]);
		end else if x < y then begin
			for i := 1 to n do
				if s[i][j] = '0' then inc(score[i]);
		end else begin
			for i := 1 to n do
				if s[i][j] = '1' then inc(score[i]);
		end;

	end;

	mx := 0;
	for i := 1 to n do mx := max(mx, score[i]);

	k := 0;
	for i := 1 to n do
		if score[i] = mx then begin
			inc(k);
			ans[k] := i;
		end;

	for i := 1 to k-1 do write(ans[i], ' ');
	writeln(ans[k]);
end.

```
