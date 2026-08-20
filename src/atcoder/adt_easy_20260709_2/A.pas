program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	r, m: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	r := 1;
	while s[r] <> 'R' do inc(r);

	m := 1;
	while s[m] <> 'M' do inc(m);

	if r < m then
		writeln('Yes')
	else
		writeln('No');
end.
