program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);
	s := 'HelloWorld';

	for i := 1 to length(s) do
		if i <> x then write(s[i]);
	writeln;
end.
