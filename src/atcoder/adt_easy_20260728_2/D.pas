program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	mx: int8;
	s: string;
	ch: char;
	c: array ['a' .. 'z'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	for ch in s do inc(c[ch]);

	mx := 0;
	for ch := 'a' to 'z' do mx := max(mx, c[ch]);

	for ch in s do
		if c[ch] < mx then write(ch);
	writeln;
end.
