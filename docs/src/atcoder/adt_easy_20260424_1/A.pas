program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, h: int8;
	s: string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	h := pos('/', s);
	ans := odd(n) and (h > 0) and (h = (n+1) div 2);

	for i := 1 to h-1 do
		ans := ans and (s[i] = '1');

	for i := h+1 to n do
		ans := ans and (s[i] = '2');

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
