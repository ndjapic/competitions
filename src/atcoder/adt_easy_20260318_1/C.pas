program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 4;
var
	i: int8;
	x: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	i := 1;
	while (i < n) and (x[i] = x[i+1]) do inc(i);

	if i < n then begin

		i := 1;
		while (i < n) and ((ord(x[i+1]) - ord(x[i]) + 10) mod 10 = 1) do inc(i);

	end;

	if i < n then
		writeln('Strong')
	else
		writeln('Weak');
end.
