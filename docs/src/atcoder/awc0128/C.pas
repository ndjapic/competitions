program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 500 * 1000;
var
	n, m, i, j, l, r: int32;
	c, cj, s, ans: int64;
	w: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(w[i]);

	read(c);
	for j := 2 to m do begin
		read(cj);
		c := min(c, cj);
	end;

	l := 1;
	s := 0;
	ans := 0;

	for r := 1 to n do begin
		inc(s, w[r]);
		while s > c do begin
			dec(s, w[l]);
			inc(l);
		end;
		inc(ans, r-l+1);
	end;

	writeln(ans);
end.
