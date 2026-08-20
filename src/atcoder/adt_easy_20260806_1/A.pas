program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch: char;
	ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	ans := 0;
	for ch in s do
		if ch = 'A' then
			ans := ans or 4
		else if ch = 'B' then
			ans := ans or 2
		else if ch = 'C' then
			ans := ans or 1;

	if ans = 7 then
		writeln('Yes')
	else
		writeln('No');
end.
