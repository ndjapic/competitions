program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for ch in s do
		if ('0' <= ch) and (ch <= '9') then
			write(ch);
	writeln;
end.
