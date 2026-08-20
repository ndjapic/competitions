program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i, h, ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		read(h);
		if m >= 0 then dec(m, h);
		if m >= 0 then ans := i;
	end;
	readln;

	writeln(ans);
end.
