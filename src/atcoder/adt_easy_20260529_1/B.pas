program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, l, r: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	l := 1;
	r := n;
	while s[l] <> '|' do inc(l);
	while s[r] <> '|' do dec(r);

	i := l+1;
	while (i < r) and (s[i] <> '*') do inc(i);

	if i < r then
		writeln('in')
	else
		writeln('out');
end.
