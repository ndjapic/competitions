program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	mm, dd, y, m, d: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	Readln(mm, dd);
	Readln(y, m, d);

	inc(d);

	if d > dd then begin
		d := 1;
		inc(m);
	end;

	if m > mm then begin
		m := 1;
		inc(y);
	end;

	writeln(y, ' ', m, ' ', d);
end.
