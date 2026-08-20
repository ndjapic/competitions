program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	t, x, i, a0, a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(t, x);

	read(a0);
	writeln('0 ', a0);

	for i := 1 to t do begin
		read(a);
		if abs(a - a0) >= x then begin
			writeln(i, ' ', a);
			a0 := a;
		end;
	end;
end.
