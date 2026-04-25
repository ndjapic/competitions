program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #naive #unsolved #tle
const
	nn = 200 * 1000;
var
	n, i, l, r, da, db, dc: int32;
	ans: int64;
	s: string;
	a, b, c: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	a[0] := 0;
	b[0] := 0;
	c[0] := 0;
	for i := 1 to n do begin
		a[i] := a[i-1];
		b[i] := b[i-1];
		c[i] := c[i-1];

		case s[i] of
			'A': inc(a[i]);
			'B': inc(b[i]);
			'C': inc(c[i]);
		end;
	end;

	ans := 0;
	for l := 0 to n-1 do
		for r := l+1 to n do begin
			da := a[r] - a[l];
			db := b[r] - b[l];
			dc := c[r] - c[l];
			if (da <> db) and (db <> dc) and (dc <> da) then inc(ans);
		end;
	writeln(ans);
end.
