program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 100;
var
	notc, tci, n, i: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do
			a[i] := (2*i-1) * (2*i+1);

		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);

	end;
end.
