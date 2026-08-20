program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	p, a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, p);

	ans := 0;
	for i := 1 to n do begin
		read(a);
		if a < p then inc(ans);
	end;
	readln;
	writeln(ans);
end.
