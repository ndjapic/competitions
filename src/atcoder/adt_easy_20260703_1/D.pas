program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #ascii #art
var
	h, w, i, j, a: int8;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do begin
		for j := 1 to w do begin
			read(a);
			if a = 0 then
				ch := '.'
			else
				ch := chr(64 + a);
			write(ch);
		end;
		writeln;
	end;
end.
