program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, ans: int8;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(a[1], a[2]);

	ans := 0;
	for i := 3 to n do begin
		read(a[i]);
		if (a[i-2] < a[i-1]) and (a[i-1] > a[i]) then inc(ans);
	end;
	readln;

	writeln(ans);
end.
