program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);
	readln(t);

	ans := 0;
	for i := 1 to n do
		if s[i] <> t[i] then inc(ans);

	writeln(ans);
end.
