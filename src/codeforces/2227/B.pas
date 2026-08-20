program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, c: int32;
	ch: char;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		c := 0;
		for ch in s do
			case ch of
				'(': inc(c);
				')': dec(c);
			end;

		if c = 0 then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
