program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, i: int32;
	d, r, t, s: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, t);
	s := 0;

	for i := 1 to n do begin
		readln(d, r);
		if r >= d*k then inc(s, r);
	end;

	if s >= t then
		writeln('Yes')
	else
		writeln('No');
end.
