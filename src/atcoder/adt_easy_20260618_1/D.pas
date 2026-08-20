program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch: char;
	c: array ['a' .. 'z'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for ch := 'a' to 'z' do c[ch] := 0;

	readln(s);

	for ch in s do inc(c[ch]);


	for ch := 'a' to 'z' do
		if c[ch] = 1 then writeln(ch);
end.
