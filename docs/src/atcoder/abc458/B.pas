program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, j, c: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do begin
		for j := 1 to w do begin
			c := 0;
			if i > 1 then inc(c);
			if j > 1 then inc(c);
			if i < h then inc(c);
			if j < w then inc(c);
			write(c);
			if j < w then write(' ');
		end;
		writeln;
	end;
end.
