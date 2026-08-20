program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	INF = 1000 * 1000 * 1000 + 1;
var
	n: int32;
	m, i: int8;
	x, p: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	x := 0;
	p := 1;
	i := 0;
	while (i <= m) and (x < INF) do begin
		inc(x, p);
		p := p * n;
		inc(i);
	end;

	if x < INF then
		writeln(x)
	else
		writeln('inf');
end.
