program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 500 * 1000;
var
	n, i: int32;
	x, s: int64;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	for i := 1 to n do read(a[i]);
	readln;

	s := 0;
	i := 0;
	while (i < n) and (s < x) do begin
		inc(i);
		inc(s, a[i]);
	end;

	if s < x then i := -1;
	writeln(i);
end.
