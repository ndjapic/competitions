program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, m, i, j: int32;
	space: boolean;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	i := 1;
	space := false;
	for j := 1 to m do
		if s[i] = t[j] then begin
			if space then
				write(' ')
			else
				space := true;

			write(j);
			inc(i);
		end;
	writeln;
end.
