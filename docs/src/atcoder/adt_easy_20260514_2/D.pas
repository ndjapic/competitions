program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: array ['1' .. '3'] of string;
	ch: char;
	t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for ch := '1' to '3' do readln(s[ch]);
	readln(t);

	for ch in t do write(s[ch]);
	writeln;
end.
