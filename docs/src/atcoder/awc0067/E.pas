program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bounded #nim
var
	n, i, k, a, x: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	x := 0;
	for i := 1 to n do begin
		read(a);
		x := x xor (a mod (k+1));
	end;
	readln;

	if x > 0 then
		writeln('Takahashi')
	else
		writeln('Aoki');
end.
