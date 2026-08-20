program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	d, mx: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	mx := -1;
	d := n mod 10;
	while (n > 0) and (mx < d) do begin
		mx := d;
		n := n div 10;
		d := n mod 10;
	end;

	if n = 0 then
		writeln('Yes')
	else
		writeln('No');
end.
