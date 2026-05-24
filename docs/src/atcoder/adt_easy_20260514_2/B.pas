program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, x, a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	for i := 1 to n do begin
		read(a);
		if a < x then begin
			x := a;
			writeln(1);
		end else
			writeln(0);
	end;
	readln;
end.
