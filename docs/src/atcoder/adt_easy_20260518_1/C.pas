program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	MM = 100;
var
	n, m, i, j: int8;
	c: array [1 .. MM] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do c[j] := 0;

	for i := 1 to n do begin
		read(j);
		inc(c[j]);
	end;
	readln;

	j := 1;
	while (j <= m) and (c[j] <= 1) do inc(j);

	if j <= m then
		writeln('No')
	else
		writeln('Yes');

	j := 1;
	while (j <= m) and (c[j] >= 1) do inc(j);

	if j <= m then
		writeln('No')
	else
		writeln('Yes');
end.
