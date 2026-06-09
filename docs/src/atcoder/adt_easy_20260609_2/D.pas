program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Math;
var
	n, i, a, x: int8;
	s: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	x := 0;
	for i := 1 to n do begin
		read(a);
		inc(s, a);
		x := max(x, a);
	end;
	readln;

	dec(s, x);
	inc(s, min(s, x));

	writeln(s div 2);
end.
