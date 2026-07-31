program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);

	i := 1;
	while (i < n) and (s[i] = t[i]) do inc(i);

	if (i < n) and (s[i+1] = t[i]) then begin
		s[i+1] := s[i];
		s[i] := t[i];
	end;

	while (i <= n) and (s[i] = t[i]) do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
