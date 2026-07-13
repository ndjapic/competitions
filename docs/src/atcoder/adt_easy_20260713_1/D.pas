program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #WA
const
	NN = 100;
var
	n, i, j: int8;
	ch: char;
	s: string;
	last: array ['a' .. 'z'] of int8;
	c: array [0 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for ch := 'a' to 'z' do last[ch] := 0;

	c[0] := 0;
	for i := 1 to n do begin
		ch := s[i];
		j := last[ch];
		c[i] := c[j] + 1;
		last[ch] := i;
	end;

	ch := 'a';
	while (ch <= 'z') and ((last[ch] = 0) or (c[last[ch]] = 2)) do inc(ch);

	if ch > 'z' then
		writeln('Yes')
	else
		writeln('No');
end.
