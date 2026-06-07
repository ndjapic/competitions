program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i: int32;
	k, e: int8;
	ans: int64;
	s: array [0 .. 2] of string;
	c, c0, c1: array [0 .. 2] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);
		readln(s[0]);
		readln(s[2]);
		setlength(s[1], n);

		for e := 0 to 2 do c1[e] := 0;

		for i := 1 to n do begin
			if s[0][i] = s[2][i] then
				s[1][i] := '0'
			else
				s[1][i] := '1';

			for e := 0 to 2 do
				if s[e][i] = '1' then inc(c1[e]);
		end;

		for e := 0 to 2 do c0[e] := n - c1[e];

		c[0] := ((1 shl k) + 2) div 3;
		c[1] := ((1 shl k) + 1) div 3;
		c[2] := c[0];

		ans := 0;
		for e := 0 to 2 do
			inc(ans, int64(c[e]) * c1[e] * c0[e]);

		writeln(ans);

	end;
end.
