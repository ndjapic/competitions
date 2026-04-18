program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 20;
var
	n, i, c, ans: int8;
	mask: int32;
	l, x: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 0 to n-1 do begin
		read(l[i]);
		inc(l[i], l[i]);
	end;
	readln;

	ans := 0;
	for mask := 0 to (1 shl n) - 1 do begin
		x[0] := 1;
		c := 0;
		for i := 0 to n-1 do begin
			if odd(mask shr i) then
				x[i+1] := x[i] + l[i]
			else
				x[i+1] := x[i] - l[i];

			if (x[i] < 0) and (x[i+1] > 0) then inc(c);
			if (x[i] > 0) and (x[i+1] < 0) then inc(c);
		end;
		ans := max(ans, c);
	end;

	writeln(ans);
end.
