program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 50;
	ww = 100 * 100;
var
	n, i: int8;
	s, t, j, p, c, w, ans: int32;
	dpt, dpa: array [0 .. nn, 0 .. ww] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, t);

	for j := 0 to s do begin
		dpt[0, j] := 0;
		dpa[0, j] := 0;
	end;

	ans := n+1;
	for i := 1 to n do begin

		for j := 0 to s do begin
			dpt[i, j] := dpt[i-1, j];
			dpa[i, j] := dpa[i-1, j];
		end;

		readln(p, c, w);

		for j := w to s do
			if (dpt[i, j] < dpt[i-1, j-w] + p-c) {and (dpa[i, j] >= dpa[i-1, j-w] + 1)} then begin
				dpt[i, j] := dpt[i-1, j-w] + p-c;
				dpa[i, j] := dpa[i-1, j-w] + 1;
			end;

	end;

	for j := 0 to s do
		if dpt[n, j] >= t then
			ans := min(ans, dpa[n, j]);

	if ans > n then ans := -1;
	writeln(ans);
end.
