program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i, h, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	ans := 0;
	for i := 1 to n do begin
		read(h);
		if m >= h then inc(ans);
		dec(m, h);
	end;
	readln;

	writeln(ans);
end.
