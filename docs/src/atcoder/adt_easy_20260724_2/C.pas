program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch, ans: char;
	c: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	for ch in s do inc(c[ch]);

	ans := 'a';
	for ch := 'b' to 'z' do
		if c[ans] < c[ch] then ans := ch;

	writeln(ans);
end.
