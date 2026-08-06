program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 50;
	INF = NN;
var
	n, a, b, x, y, z, l, r: int8;
	m, i: int16;
	d: array [1 .. NN, 1 .. NN] of int8;
	c: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do
		for b := 1 to n do
			if a = b then
				d[a, b] := 0
			else
				d[a, b] := INF;

	for i := 1 to m do begin
		readln(a, b);
		d[a, b] := 1;
	end;

	for y := 1 to n do
		for x := 1 to n do
			for z := 1 to n do
				if d[x, z] > d[x, y] + d[y, z] then
					d[x, z] := d[x, y] + d[y, z];

	for a := 1 to n do begin
		c[a] := 0;
		for b := 1 to n do
			if d[a, b] < INF then inc(c[a]);
	end;

	l := 1;
	r := n;
	while (l <= r) and (c[r] < n) do dec(r);
	while (l <= r) and (c[l] < n) do inc(l);

	if (r > 0) and (l = r) then
		writeln(r)
	else
		writeln(-1);
end.
