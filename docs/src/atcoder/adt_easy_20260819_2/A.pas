program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	e, w: int8;
	ch: char;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	e := 0;
	w := 0;

	for ch in s do
		case ch of
			'E': inc(e);
			'W': inc(w);
		end;

	if e > w then
		writeln('East')
	else
		writeln('West');
end.
