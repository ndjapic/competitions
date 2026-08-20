program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, t: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	t := -1;
	while k > 0 do begin
		inc(t);
		dec(k, n+t);
	end;

	writeln(t);
end.
