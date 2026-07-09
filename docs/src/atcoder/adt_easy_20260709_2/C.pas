program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	for i := 1 to n do
		if (i < n) and (copy(s, i, 2) = 'na') then
			write('ny')
		else
			write(s[i]);
	writeln;
end.
