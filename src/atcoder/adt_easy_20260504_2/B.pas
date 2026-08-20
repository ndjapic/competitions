program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	m, d: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(m, d);

	if not odd(m) then
		writeln('No')
	else if m > 9 then
		writeln('No')
	else if m = 1 then begin
		if d = 7 then
			writeln('Yes')
		else
			writeln('No');
	end else if m = d then
		writeln('Yes')
	else
		writeln('No');
end.
