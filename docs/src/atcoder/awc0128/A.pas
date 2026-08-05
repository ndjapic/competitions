program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, i, a, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	ans := 0;
	for i := 1 to n do begin
		read(a);
		if a mod k = 0 then inc(ans);
	end;
	readln;

	writeln(ans);
end.
