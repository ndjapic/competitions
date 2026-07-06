program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, s, k, p, q, i, pay: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, k);

	pay := 0;
	for i := 1 to n do begin
		readln(p, q);
		inc(pay, p * q);
	end;

	if pay < s then inc(pay, k);
	writeln(pay);
end.
