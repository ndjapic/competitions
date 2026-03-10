program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	mm = 200 * 1000;
var
	n, m, i, j, k, kk, recipient: int32;
	p: array [1 .. mm] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do read(p[j]);
	readln;

	for i := 1 to n do begin
		recipient := 0;
		read(kk);
		for k := 1 to kk do begin
			read(j);

			if (recipient = 0) or
				(p[j] > p[recipient]) or
				(p[j] = p[recipient]) and
				(j < recipient)
			then
				recipient := j;
		end;
		readln;
		writeln(recipient);
	end;
end.
