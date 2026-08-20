program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, m, mn, i: int32;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);
	mn := min(n, m);

	i := 1;
	while (i <= mn) and (s[i] = t[i]) do inc(i);

	if (i > mn) and (n = m) then i := 0;
	writeln(i);
end.
