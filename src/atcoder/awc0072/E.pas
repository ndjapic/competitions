program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #prefix #count
const
	NN = 200 * 1000;
var
	n, m, i, j, l, r: int32;
	s, p: array [1 .. NN] of int32;
	c: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(s[i]); readln;
	for i := 1 to n do read(p[i]); readln;

	c[0] := 0;
	for i := 1 to n do begin
		c[i] := c[i-1];
		if s[i] < p[i] then inc(c[i]);
	end;

	for j := 1 to m do begin
		readln(l, r);
		if c[l-1] = c[r] then
			writeln('Yes')
		else
			writeln('No');
	end;
end.
