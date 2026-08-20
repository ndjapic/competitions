program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	k, h, m: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k);
	h := 21 + k div 60;
	m := k mod 60;

	if h < 10 then write('0');
	write(h, ':');
	if m < 10 then write('0');
	writeln(m);
end.
