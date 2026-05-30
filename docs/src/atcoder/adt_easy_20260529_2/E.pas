program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 20;
var
	n, i, c, ans: int8;
	mask: int32;
	l: array [0 .. NN] of int32;
	x: array [0 .. NN] of int64;
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

	x[0] := 1;
	ans := 0;
	for mask := 0 to (1 shl n) - 1 do begin
		c := 0;
		for i := 0 to n-1 do begin
			if odd(mask shr i) then
				x[i+1] := x[i] - l[i]
			else
				x[i+1] := x[i] + l[i];

			if x[i] < 0 then begin
				if x[i+1] > 0 then inc(c);
			end else if x[i+1] < 0 then
				inc(c);
		end;
		ans := max(ans, c);
	end;

	writeln(ans);
end.
