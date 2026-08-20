program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 20;
var
	n, k, i, nobs: int8;
	s, mask, mx: int32;
	f, b: array [0 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 0 to n-1 do readln(f[i], b[i]);

	mx := 0;
	for mask := 0 to (1 shl n) - 1 do begin
		nobs := 0;
		s := 0;
		for i := 0 to n-1 do
			if odd(mask shr i) then begin
				inc(nobs);
				inc(s, b[i]);
			end else
				inc(s, f[i]);

		if nobs = k then
			mx := max(mx, s);
	end;

	writeln(mx);
end.
