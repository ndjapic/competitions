program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k: int8;
	a, inf, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	inf := 1;
	for i := 1 to k do inf := inf * 10;
	dec(inf);

	ans := 1;
	for i := 1 to n do begin
		read(a);
		if ans <= inf div a then
			ans := ans * a
		else
			ans := 1;
	end;
	readln;

	writeln(ans);
end.
