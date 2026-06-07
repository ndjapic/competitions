program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci: int32;
	n, a, b: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		a := n mod 12;
		if a = 10 then a := 22;
		b := n-a;

		if b < 0 then
			writeln('-1')
		else
			writeln(a, ' ', b);

	end;
end.
