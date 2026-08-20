program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000 * 1000;
var
	notc, tci, n, l, r: int32;
	m: int8;
	beauty: int64;
	s: string;
	c: array [0 .. NN, '0' .. '1'] of int32;
	d: array [0 .. NN, 0 .. 2] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		r := 0;
		c[r, '0'] := 0;
		c[r, '1'] := 0;

		for m := 0 to 2 do d[r, m] := 0;
		m := (c[r, '1'] - c[r, '0'] + 3 shl 20) mod 3;
		inc(d[r, m]);

		beauty := int64(n+1) * n div 2;
		l := 1;

		for r := 1 to n do begin
			if (r = 1) or (s[r-1] = s[r]) then l := r;

			c[r, '0'] := c[r-1, '0'];
			c[r, '1'] := c[r-1, '1'];
			inc(c[r, s[r]]);

			for m := 0 to 2 do d[r, m] := d[r-1, m];
			m := (c[r, '1'] - c[r, '0'] + 3 shl 20) mod 3;
			dec(beauty, d[r, m] + (r-l) div 2);
			inc(d[r, m]);
		end;

		writeln(beauty);

	end;
end.
