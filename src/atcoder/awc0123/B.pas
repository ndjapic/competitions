program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #joke
var
	n, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	ans := 0;
	for i := 1 to n do
		if s[i] = 'o' then inc(ans);

	writeln(ans);
end.
