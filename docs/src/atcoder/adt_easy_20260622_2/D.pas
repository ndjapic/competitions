program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, l, r, a, b, a0, b0: int32;
	s: string;
	c: array [0 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	c[0] := 0;
	a0 := 0;
	b0 := 1;
	for r := 1 to n do begin
		c[r] := c[r-1];
		if s[r] = 't' then inc(c[r]);

		for l := 1 to r do begin
			a := c[r] - c[l-1] - 2;
			b := r-l+1 - 2;

			if (b >= 1) and (s[l] = 't') and (s[r] = 't') and (a * b0 > b * a0) then begin
				a0 := a;
				b0 := b;
			end;
		end;
	end;

	writeln(a0 / b0 :0:9);
end.
