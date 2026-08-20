program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	q, i, k, n, tp: int8;
	x: int32;
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
				readln(x);
				inc(n);
				a[n] := x;
			end;

			2: begin
				readln(k);
				writeln(a[n+1-k]);
			end;

		end;
	end;
end.
