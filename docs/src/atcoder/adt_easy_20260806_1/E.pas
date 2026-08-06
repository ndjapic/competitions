program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, l, r: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	l := 1;
	r := n;

	while (l < r) and (s[r] = 'a') do begin
		dec(r);
		if (l < r) and (s[l] = 'a') then inc(l);
	end;

	while (l < r) and (s[l] = s[r]) do begin
		inc(l);
		dec(r);
	end;

	if l >= r then
		writeln('Yes')
	else
		writeln('No');
end.
