program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, k, i: int8;
	a, b: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do begin
		read(a[i]);
		b[i] := 0;
		if i > k then
			b[i-k] := a[i];
	end;
	readln;

	for i := 1 to n-1 do write(b[i], ' ');
	writeln(b[n]);
end.
