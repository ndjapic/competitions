program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i, s, t: int32;
	ans: int64;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	ans := a[1];
	for i := 1 to n-1 do begin
		readln(s, t);
		ans := ans div s * t + a[i+1];
	end;

	writeln(ans);
end.
