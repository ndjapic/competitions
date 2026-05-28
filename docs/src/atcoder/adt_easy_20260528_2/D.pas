program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	q, i, n, tp, k: int8;
	a: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(q);

	n := 0;
	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				inc(n);
				readln(a[n]);
			end;

			2: begin
				readln(k);
				writeln(a[n-k+1]);
			end;

		end;
	end;
end.
