program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, i0: int8;
	h: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	i0 := 1;
	for i := 1 to n do begin
		read(h[i]);
		if h[i] > h[i0] then i0 := i;
	end;
	readln;

	writeln(i0);
end.
