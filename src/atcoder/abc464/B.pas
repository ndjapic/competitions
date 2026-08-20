program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 50;
var
	h, w, i, j, il, ir, jl, jr: int8;
	c: array [1 .. HH] of string;
	white: array [1 .. HH] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do begin
		readln(c[i]);

		white[i] := true;
		for j := 1 to w do
			if c[i][j] = '#' then white[i] := false;
	end;

	il := 1;
	ir := h;
	while white[il] do inc(il);
	while white[ir] do dec(ir);

	for j := 1 to w do begin
		white[j] := true;
		for i := il to ir do
			if c[i][j] = '#' then white[j] := false;
	end;

	jl := 1;
	jr := w;
	while white[jl] do inc(jl);
	while white[jr] do dec(jr);

	for i := il to ir do
		writeln(copy(c[i], jl, jr - jl + 1));
end.
