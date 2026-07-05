program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int8;
	a, l: int16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l);

	ans := 0;
	for i := 1 to n do begin
		read(a);
		if a >= l then inc(ans);
	end;
	readln;

	writeln(ans);
end.
