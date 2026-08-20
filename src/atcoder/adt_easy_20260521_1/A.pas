program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 3;
var
	i: int8;
	ch: char;
	c: array ['A' .. 'C'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for ch := 'A' to 'C' do c[ch] := 0;

	for i := 1 to N do begin
		read(ch);
		if ch <= 'C' then inc(c[ch]);
	end;
	readln;

	ch := 'A';
	while (ch <= 'C') and (c[ch] = 1) do inc(ch);

	if ch > 'C' then
		writeln('Yes')
	else
		writeln('No');
end.
