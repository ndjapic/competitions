program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 5;
var
	i, l, r: int8;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for i := 1 to NN do read(a[i]);
	readln;

	l := 1;
	r := NN;
	while (l < r) and (a[l] = l) do inc(l);
	while (l < r) and (a[r] = r) do dec(r);

	if r-l = 1 then
		writeln('Yes')
	else
		writeln('No');
end.
