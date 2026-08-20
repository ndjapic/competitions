program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i, a, b: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(a, b);

	for i := 1 to length(s) do
		if i = a then
			write(s[b])
		else if i = b then
			write(s[a])
		else
			write(s[i]);

	writeln;
end.
