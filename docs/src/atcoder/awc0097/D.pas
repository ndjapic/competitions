program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid #rectangular #subarray #sum #kadane
uses
	math;
const
	NN = 500;
var
	n, m, i, r, c: int32;
	a: array [1 .. NN, 1 .. NN] of int64;
	s, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := low(int64);
	for r := 1 to n do begin

		for c := 1 to m do begin
			read(a[r, c]);
			for i := 1 to r-1 do inc(a[i, c], a[r, c]);
		end;
		readln;

		for i := 1 to r do begin
			s := 0;
			for c := 1 to m do begin
				s := max(s, 0) + a[i, c];
				ans := max(ans, s);
			end;
		end;

	end;

	writeln(ans);
end.
