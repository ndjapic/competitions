program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a: int8;
	m: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		read(a);
		dec(m, a);
	end;
	readln;

	if m >= 0 then
		writeln('Yes')
	else
		writeln('No');
end.
