program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	ch, ans: char;
	s: string;
	c: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	for i := 1 to n do inc(c[s[i]]);

	ans := 'a';
	for ch := 'b' to 'z' do
		if c[ch] > c[ans] then ans := ch;

	writeln(ans);
end.
