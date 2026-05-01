program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s, t: string;
	l, r: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);

	l := 1;
	r := length(s);
	while (l <= r) and (s[l] = t[l]) do inc(l);
	while (l <= r) and (s[r] = t[r]) do dec(r);

	if (l > r) or (r-l = 1) and (s[l] = t[r]) and (s[r] = t[l]) then
		writeln('Yes')
	else
		writeln('No');
end.
