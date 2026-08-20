program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	classes, sysutils;
var
	n, i, a, b: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	b := 0;
	for i := 1 to n do begin
		read(a);
		b := a xor b;
	end;
	readln;

	if b mod 2 = 1 then
		writeln('Takahashi')
	else
		writeln('Aoki');
end.
