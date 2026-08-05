program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int8;
	h, x, p: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, h, x);

	ans := 0;
	for i := 1 to n do begin
		read(p);
		if (ans = 0) and (h+p >= x) then ans := i;
	end;
	readln;

	writeln(ans);
end.
