program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	S: string = 'HelloWorld';
	N = 10;
var
	x, i: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	for i := 1 to N do
		if i <> x then write(S[i]);
	writeln;
end.
