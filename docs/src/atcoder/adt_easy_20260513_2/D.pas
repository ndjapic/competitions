program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, j: int32;
	s, t: string;
	ch: char;
	space: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	space := false;
	j := 0;
	for ch in s do begin
		inc(j);
		while t[j] <> ch do inc(j);
		if space then
			write(' ')
		else
			space := true;
		write(j);
	end;
	writeln;
end.
