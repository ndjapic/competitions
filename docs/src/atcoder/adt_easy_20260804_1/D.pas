program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	s: string;
	ch: char;
	n, i: int8;
	c1: array ['a' .. 'z'] of int8;
	c2: array [0 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for ch := 'a' to 'z' do c1[ch] := 0;

	for ch in s do inc(c1[ch]);

	for i := 0 to n do c2[i] := 0;

	for ch := 'a' to 'z' do inc(c2[c1[ch]]);

	i := 1;
	while (i <= n) and ((c2[i] = 0) or (c2[i] = 2)) do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
