program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 10;
var
	n, a, b, i, j: int8;
	wh, bl: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);

	setlength(wh, b);
	setlength(bl, b);
	for j := 1 to b do begin
		wh[j] := '.';
		bl[j] := '#';
	end;

	for i := 0 to a*n-1 do begin
		for j := 0 to n-1 do
			if odd(i div a mod 2 + j mod 2) then
				write(bl)
			else
				write(wh);
		writeln;
	end;
end.
