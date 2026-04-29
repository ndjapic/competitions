program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s, t: string;
	similar: boolean;
	x, y: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);
	readln(t);

	i := 0;
	similar := true;
	while (i < n) and similar do begin
		inc(i);
		x := s[i];
		y := t[i];

		if x = '1' then
			x := 'l'
		else if x = '0' then
			x := 'o';

		if y = '1' then
			y := 'l'
		else if y = '0' then
			y := 'o';

		similar := x = y;
	end;

	if similar then
		writeln('Yes')
	else
		writeln('No');
end.
