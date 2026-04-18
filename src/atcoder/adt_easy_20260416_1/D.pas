program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i: int8;
	s: string;
	ch: char;
	c1: array ['a' .. 'z'] of int8;
	c2: array [0 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for ch := 'a' to 'z' do c1[ch] := 0;
	for i := 1 to n do inc(c1[s[i]]);

	for i := 0 to n do c2[i] := 0;
	for ch := 'a' to 'z' do inc(c2[c1[ch]]);

	i := 1;
	while (i <= n) and ((c2[i] = 0) or (c2[i] = 2)) do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
