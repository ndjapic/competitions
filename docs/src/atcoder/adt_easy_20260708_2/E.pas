program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #queue #query
const
	NN = 300 * 1000;
var
	q, i, l, k, head, tail: int32;
	tp: int8;
	snakes: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(q);

	head := 0;
	tail := 0;
	snakes[0] := 0;

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(l);
				inc(tail);
				snakes[tail] := snakes[tail - 1] + l;
			end;

			2: inc(head);

			3: begin
				read(k);
				writeln(snakes[head + k-1] - snakes[head]);
			end;

		end;
		readln;
	end;
end.
