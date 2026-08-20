program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	r, c, i, j, a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, c);

	for i := 1 to 2 do begin
		for j := 1 to 2 do begin
			read(a);
			if (i = r) and (j = c) then writeln(a);
		end;
		readln;
	end;
end.
