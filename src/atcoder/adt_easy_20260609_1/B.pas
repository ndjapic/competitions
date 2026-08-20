program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	d, f, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(d, f);
	ans := (f+7 - d mod 7) mod 7;
	if ans = 0 then ans := 7;
	writeln(ans);
end.
