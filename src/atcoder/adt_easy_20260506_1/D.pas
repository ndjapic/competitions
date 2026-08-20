program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	if x < 0 then begin
		ans := -((-x) div 10);
	end else
		ans := (x+9) div 10;

	writeln(ans);
end.
