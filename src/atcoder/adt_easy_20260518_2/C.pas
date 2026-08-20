program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, j, a: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do begin
		for j := 1 to w do begin
			read(a);
			if a = 0 then
				write('.')
			else
				write(chr(ord('A') + a-1));
		end;
		readln;
		writeln;
	end;
end.
