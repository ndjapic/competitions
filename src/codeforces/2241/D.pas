program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	notc, tci, n, i: int32;
	d: int64;
	a, b: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;
		for i := 1 to n do read(b[i]); readln;

		i := 1;
		d := 0;
		while (i <= n) and (d >= 0) do begin
			inc(d, b[i] - a[i]);
			inc(i);
		end;

		if d >= 0 then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
