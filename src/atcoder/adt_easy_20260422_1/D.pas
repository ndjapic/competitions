program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, p, i, ai, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, p);

	ans := 0;
	for i := 0 to n-1 do begin
		read(ai);
		if ai < p then inc(ans);
	end;
	readln;

	writeln(ans);
end.
