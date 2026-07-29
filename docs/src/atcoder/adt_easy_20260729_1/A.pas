program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	N = 3;
var
	i: int8;
	p: int32;
	a: array [1 .. N] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	p := 1;
	for i := 1 to N do begin
		read(a[i]);
		p := p * a[i];
	end;
	readln;

	i := 1;
	while (i <= 3) and (sqr(a[i]) <> p) do inc(i);

	if i <= 3 then
		writeln('Yes')
	else
		writeln('No');
end.
