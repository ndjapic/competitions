program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #tree #binary #complete #default #indexing #Ahnentafel #Sosa-Stradonitz #Aitzing
var
	a, b: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	if (a div 2 = b) or (b div 2 = a) then
		writeln('Yes')
	else
		writeln('No');
end.
