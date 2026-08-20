program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	k: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);

	n := length(s);
	i := 1;
	k := (ord(t[i]) - ord(s[i]) + 26) mod 26;
	while (i <= n) and ( k = (ord(t[i]) - ord(s[i]) + 26) mod 26 ) do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
