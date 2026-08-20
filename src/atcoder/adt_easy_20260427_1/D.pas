program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, t, i, w: int32;
	trump: boolean;
	c, r: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, t);

	trump := false;
	for i := 1 to n do begin
		read(c[i]);
		if c[i] = t then trump := true;
	end;
	readln;

	if not trump then t := c[1];

	w := 0;
	r[0] := 0;
	for i := 1 to n do begin
		read(r[i]);
		if c[i] <> t then r[i] := 0;
		if r[w] < r[i] then w := i;
	end;
	readln;

	writeln(w);
end.
