program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, c, c1: int32;
	ans, ans1: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		c1 := 0;
		ans := 0;
		ans1 := 0;
		for i := 1 to n do begin
			read(c);
			if c = 1 then
				inc(c1)
			else begin
				inc(ans, c);
				inc(ans1, c div 2);
			end;
		end;
		readln;

		if c1 < n-1 then dec(ans1, n-c1);
		if (ans < 3) and (c1 = 0) then ans := 0;
		writeln(ans + min(ans1, c1));

	end;
end.
