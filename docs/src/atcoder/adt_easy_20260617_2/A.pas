program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #linear #search
const
	NN = 100;
var
	n, i: int8;
	a: array [0 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	i := 1;
	while (i+2 <= n) and not ((a[i] = a[i+1]) and (a[i] = a[i+2])) do inc(i);

	if i+2 <= n then
		writeln('Yes')
	else
		writeln('No');
end.
