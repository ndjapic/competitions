program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, i1: int8;
	h, h1: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	i1 := 1;
	read(h1);
	for i := 2 to n do begin
		read(h);
		if h1 < h then begin
			i1 := i;
			h1 := h;
		end;
	end;
	readln;

	writeln(i1);
end.
