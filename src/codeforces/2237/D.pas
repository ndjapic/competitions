program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000 * 1000;
var
	notc, tci, n, l, r: int32;
	m: int8;
	beauty: int64;
	s: string;
	c: array [0 .. 2] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		c[0] := 1;
		c[1] := 0;
		c[2] := 0;
		m := 0;
		beauty := int64(n+1) * n div 2;

		for r := 1 to n do begin
			if (r = 1) or (s[r-1] = s[r]) then l := r;
			m := (m + ord('2') - ord(s[r])) mod 3;
			dec(beauty, c[m] + (r-l) div 2);
			inc(c[m]);
		end;

		writeln(beauty);

	end;
end.
