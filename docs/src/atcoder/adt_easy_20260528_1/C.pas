program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x: string;
	n, i: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;
begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);
	n := length(x);

	while x[n] = '0' do dec(n);
	if x[n] = '.' then dec(n);

	for i := 1 to n do write(x[i]);
	writeln;
end.
