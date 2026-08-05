program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, l, r: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	l := 1;
	r := n;
	while (l < r) and (s[l] = '1') and (s[r] = '2') do begin
		inc(l);
		dec(r);
	end;

	if (l = r) and (s[l] = '/') then
		writeln('Yes')
	else
		writeln('No');
end.
