program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	ans := 0;
	if s[1] = 'o' then inc(ans);
	if s[n] = 'i' then inc(ans);

	for i := 2 to n do
		if s[i-1] = s[i] then inc(ans);

	writeln(ans);
end.
