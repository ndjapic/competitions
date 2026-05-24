program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	j: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 0 to n do begin
		j := 1;
		while (j <= 9) and not ((n mod j = 0) and (i mod (n div j) = 0)) do inc(j);
		if j > 9 then
			write('-')
		else
			write(j);
	end;

end.
