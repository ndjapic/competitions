program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	ans := 0;
	for i := 1 to n do
		if (s[i] = 'x') and ((i = 1) or (s[i-1] = 'x')) and ((i = n) or (s[i+1] = 'x')) then
			inc(ans);

	writeln(ans);
end.
