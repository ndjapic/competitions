program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	i := length(s);
	while (i > 0) and (s[i] <> 'a') do dec(i);

	if i = 0 then i := -1;
	writeln(i);
end.
