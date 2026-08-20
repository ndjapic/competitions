program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, p, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, p);
	dec(n);
	dec(m);

	ans := n div p;
	if m <= n mod p then inc(ans);

	writeln(ans);
end.
