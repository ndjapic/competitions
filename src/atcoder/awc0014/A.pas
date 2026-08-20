program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, r, t, p: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, r, t);

	for i := 1 to n do begin
		read(p);
		write(min(t div p, r), ' ');
	end;
	readln;
	writeln;
end.
