program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	ntc, tci: int32;
	x, y, k: int64;
	ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(x, y, k);

		ans := 0;
		while x <> y do begin
			if x > y then
				x := x div k
			else
				y := y div k;
			inc(ans);
		end;

		writeln(ans);

	end;
end.
