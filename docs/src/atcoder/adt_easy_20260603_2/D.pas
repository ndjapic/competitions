program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, m: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	m := 0;
	for i := 1 to n do
		if s[i] = '#' then begin
			write(i);
			inc(m);
			if odd(m) then
				write(',')
			else
				writeln;
		end;
end.
