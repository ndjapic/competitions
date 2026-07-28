program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 1000;
var
	n, i, j, ans1, ans2: int32;
	a, b: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	for j := 1 to n do read(b[j]);
	readln;

	ans1 := 0;
	ans2 := 0;

	for i := 1 to n do
		for j := 1 to n do
			if a[i] = b[j] then begin
				if i = j then
					inc(ans1)
				else
					inc(ans2);
			end;

	writeln(ans1);
	writeln(ans2);
end.
