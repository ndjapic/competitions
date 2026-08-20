program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	if not (('A' <= s[1]) and (s[1] <= 'Z')) then
		writeln('No')
	else begin
		i := 2;
		while (i <= n) and ('a' <= s[i]) and (s[i] <= 'z') do inc(i);
		if i <= n then
			writeln('No')
		else
			writeln('Yes');
	end;
end.
