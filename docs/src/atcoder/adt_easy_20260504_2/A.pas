program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	i := 1;
	while s[i] <> ' ' do begin
		write(s[i]);
		inc(i);
	end;

	writeln(' san');
end.
