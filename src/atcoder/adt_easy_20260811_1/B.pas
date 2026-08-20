program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, d, i, ans: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);
	readln(s);

	ans := d;
	for i := 1 to n do
		if s[i] = '.' then inc(ans);

	writeln(ans);
end.
