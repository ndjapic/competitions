program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 26;
var
	i: int8;
	ch: char;
	ans: int32;
	s: string;
	a: array ['A' .. 'Z'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for i := 1 to N do a[s[i]] := i;

	ans := 0;
	for ch := 'B' to 'Z' do
		inc(ans, abs(a[ch] - a[pred(ch)]));

	writeln(ans);
end.
