program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c, d: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c, d);

	inc(b, a * 60);
	inc(d, c * 60);

	if b <= d then
		writeln('Takahashi')
	else
		writeln('Aoki');
end.
