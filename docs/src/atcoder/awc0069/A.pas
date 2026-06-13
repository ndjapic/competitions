program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, i0, j: int8;
	a1, a2: int32;
	volatility: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	i0 := 1;
	for i := 1 to n do begin
		read(a1);
		volatility[i] := 0;
		for j := 2 to m do begin
			read(a2);
			inc(volatility[i], abs(a2 - a1));
			a1 := a2;
		end;
		readln;

		if volatility[i] > volatility[i0] then i0 := i;
	end;

	writeln(i0);
end.
