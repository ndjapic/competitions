program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j: int8;
	found: boolean;
	a, c: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do c[j] := 0;

	for i := 1 to n do begin
		read(a[i]);
		inc(c[a[i]]);
	end;
	readln;

	i := n;

	j := 1;
	while (j <= m) and (c[j] > 0) do inc(j);

	if j > m then begin
		found := false;
		while not found do begin
			dec(c[a[i]]);
			found := c[a[i]] = 0;
			dec(i);
		end;
	end;

	writeln(n-i);
end.
