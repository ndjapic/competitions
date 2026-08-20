program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	ch: char;
	s: string;
	c: array ['a' .. 'z'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	i := n div 2;
	while (i > 0) and (s[2*i-1] = s[2*i]) do dec(i);

	for ch := 'a' to 'z' do c[ch] := 0;
	for ch in s do inc(c[ch]);

	ch := 'a';
	while (ch <= 'z') and ((c[ch] = 0) or (c[ch] = 2)) do inc(ch);

	if not odd(n) and (i <= 0) and (ch > 'z') then
		writeln('Yes')
	else
		writeln('No');
end.
