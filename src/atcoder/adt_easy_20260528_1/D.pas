program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, x, s: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	s := 0;
	x := -1;
	while s < k do begin
		inc(x);
		inc(s, n+x);
	end;

	writeln(x);
end.
