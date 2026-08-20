program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, i, l, r: int8;
	s: string;
	ans, rate: double;
	ind: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	r := 0;
	ans := 0.0;
	for i := 1 to n do
		if s[i] = 't' then begin
			inc(r);
			ind[r] := i;
			for l := 1 to r-2 do begin
				rate := 1.0;
				rate := rate * (r - l - 1) / (ind[r] - ind[l] - 1);
				ans := max(ans, rate);
			end;
		end;

	writeln(ans:0:9);
end.
