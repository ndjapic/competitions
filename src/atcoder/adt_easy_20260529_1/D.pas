program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	RR = 100;
var
	q, i, tp, l, r: int32;
	que: array [1 .. RR] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(q);

	l := 1;
	r := 0;

	for i := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				inc(r);
				read(que[r]);
			end;

			2: begin
				writeln(que[l]);
				inc(l);
			end;

		end;
		readln;
	end;
end.
