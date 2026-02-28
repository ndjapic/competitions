program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
var
	notc, tci, n, i: int32;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		i := 1;
		while (i < n) and (a[i] <= a[i+1]) do inc(i);

		if i = n then
			writeln(n)
		else
			writeln(1);

	end;
end.
