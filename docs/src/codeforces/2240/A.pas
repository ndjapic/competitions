program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci, n, k, p2, d, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		ans := 0;
		p2 := 1;
		while n >= p2 do begin
			d := min(k, n div p2);
			inc(ans, d);
			dec(n, d * p2);
			inc(p2, p2);
		end;

		writeln(ans);

	end;
end.
