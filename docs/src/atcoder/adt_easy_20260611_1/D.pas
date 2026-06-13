program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 50;
var
	n, i, ans: int8;
	p: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 2 to n do read(p[i]);
	readln;

	ans := 0;
	i := n;
	while i > 1 do begin
		i := p[i];
		inc(ans);
	end;

	writeln(ans);
end.
