program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses StrUtils;
var
	s, x, y: string;
	n, p: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	p := pos(' ', s);
	x := leftstr(s, p-1);
	y := rightstr(s, n-p);
{
	case x[1] of

		'O': case y[1] of
			'O': writeln('Yes');
			'S': writeln('No');
			'L': writeln('No');
		end;

	end;
}
	if x = 'Lynx' then
		writeln('Yes')
	else if y = 'Ocelot' then
		writeln('Yes')
	else if x = y then
		writeln('Yes')
	else
		writeln('No');
end.
