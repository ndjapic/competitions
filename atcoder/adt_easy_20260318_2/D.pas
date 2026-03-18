program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, i: int8;
	b, p: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(b);

	a := 16;
	p := int64(1) shl 60;
	while p > b do begin
		dec(a);
		p := 1;
		for i := 1 to a do p := p * a;
	end;

	if p = b then
		writeln(a)
	else
		writeln(-1);
end.
