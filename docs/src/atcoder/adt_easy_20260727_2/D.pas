program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #interesting
const
	NN = 200 * 1000;
var
	n, t, i, i0: int32;
	c, r: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t);

	for i := 1 to n do read(c[i]);
	readln;

	for i := 1 to n do read(r[i]);
	readln;

	i0 := 0;
	for i := 1 to n do
		if (c[i] = t) and ((i0 = 0) or (r[i0] < r[i])) then i0 := i;

	if i0 = 0 then begin
		i0 := 1;
		for i := 2 to n do
			if (c[i] = c[1]) and (r[i0] < r[i]) then i0 := i;
	end;

	writeln(i0);
end.
