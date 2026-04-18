program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	n = 5;
var
	i, s: int8;
	a: array [1 .. n] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	read(a[1]);
	s := 0;

	for i := 2 to n do begin
		read(a[i]);
		inc(s, max(0, a[i-1] - a[i]));
	end;
	readln;

	if s = 1 then
		writeln('Yes')
	else
		writeln('No');
end.
