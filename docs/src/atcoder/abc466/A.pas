program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, x: int8;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := true;

	for i := 1 to n do begin
		read(x);
		if x >= 0 then ans := false;
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
