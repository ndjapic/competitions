program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, i, j, ans: int32;
	p: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(p[i]);
	readln;

	i := 1;
	ans := 0;
	while p[i] > 0 do begin
		j := p[i];
		p[i] := 0;
		i := j;
		inc(ans);
	end;

	writeln(ans);
end.
