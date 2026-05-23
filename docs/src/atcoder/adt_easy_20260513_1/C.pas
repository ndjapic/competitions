program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, q, i, j, x: int32;
	tp: int8;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do read(a[i]);
	readln;

	readln(q);
	for j := 1 to q do begin
		read(tp, i);
		case tp of

			1: begin
				read(x);
				a[i] := x;
			end;

			2: writeln(a[i]);

		end;
		readln;
	end;
end.
