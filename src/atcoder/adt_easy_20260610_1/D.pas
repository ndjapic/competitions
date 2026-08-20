program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	m, i, l, r: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	m := length(s);

	l := 1;
	i := 0;
	for r := 2 to m do
		if s[r] = '|' then begin
			inc(i);
			write(r-l-1);
			if r < m then write(' ');
			l := r;
		end;
end.
