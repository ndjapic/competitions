program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, w, i, j, l, s: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, w);

	j := 1;
	read(s);
	for i := 2 to n do begin
		read(l);

		if s+1+l > w then begin
			inc(j);
			s := l;
		end else
			inc(s, l+1);
	end;
	readln;

	writeln(j);
end.
