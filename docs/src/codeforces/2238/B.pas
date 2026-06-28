program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 2000;
var
	notc, tci, b: int32;
	n, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		ans := 0;
		for b := 1 to n do inc(ans, sqr(n div b));

		writeln(ans);

	end;
end.
