program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	i := 1;
	while (i <= n) and (s[i] = 'o') do inc(i);

	while i <= n do begin
		write(s[i]);
		inc(i);
	end;

	writeln;
end.
