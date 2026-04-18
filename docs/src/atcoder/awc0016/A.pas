program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, a, b, c: int32;
	s: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	c := 0;
	s := 0;

	for i := 1 to n do begin
		readln(a, b);
		if a > b then begin
			inc(c);
			inc(s, a-b);
		end;
	end;

	writeln(c, ' ', s);
end.
