program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, q, i, j, b, x: int8;
	box: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for j := 1 to n do box[j] := 0;

	for i := 1 to q do begin
		read(x);
		if x > 0 then
			b := x
		else begin
			b := 1;
			for j := 1 to n do
				if box[j] < box[b] then b := j;
		end;
		inc(box[b]);
		write(b);
		if i < q then write(' ');
	end;
	readln;
	writeln;
end.
