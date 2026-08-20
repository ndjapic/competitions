program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #tree #parent #link #toposort
const
	NN = 300 * 1000;
var
	n, i, p, b: int32;
	ans: int64;
	a: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	ans := 0;

	for i := 1 to n do read(a[i]);
	readln;

	for i := 2 to n do begin
		read(p);
		inc(a[i], a[p]);
	end;
	readln;

	read(b);
	for i := 2 to n do begin
		read(b);
		inc(ans, a[i] * b);
	end;
	readln;

	writeln(ans);
end.
