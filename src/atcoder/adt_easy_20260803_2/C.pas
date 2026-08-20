program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100;
var
	n, i, j: int8;
	total: int32;
	ch: char;
	c: array [1 .. NN] of char;
	l: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	total := 0;
	for i := 1 to n do begin
		readln(c[i], ch, l[i]);
		total := min(total + l[i], NN + 1);
	end;

	if total > NN then
		writeln('Too Long')
	else begin
		for i := 1 to n do
			for j := 1 to l[i] do write(c[i]);
		writeln;
	end;

end.
